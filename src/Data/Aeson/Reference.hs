{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Reference
  ( resolveReference
  ) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson (Value(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Read qualified as Text.Read
import Data.Vector qualified as Vector
import Network.HTTP.Types.URI (urlDecode)
import System.Directory (canonicalizePath)
import System.FilePath ((</>), isRelative, takeDirectory)


-- | A JSON Pointer (RFC 6901) used to navigate the internal structure of a
-- document, e.g. "/definitions/user/name".
type Pointer = Text


-- | An identifier for a value, potentially combining a resource location and a
-- fragment, e.g. "common.json#/schemas/address".
type Reference = Text


-- | A unique, absolute identifier for a document.
-- In the current implementation, this is a canonicalized file path.
type Resource = FilePath


-- | A lookup table that stores fully resolved documents.
-- Keyed by 'Resource' identifiers to prevent redundant fetches and
-- ensure consistency across multiple references to the same entity.
type Cache = HashMap Resource Value


-- | The internal transformation stack.
-- * 'StateT' 'Cache' persists resolved resources across recursive calls.
-- * 'ExceptT' handles resolution errors (e.g., missing keys, retrieval failures).
-- * 'IO' is used for the side-effects of fetching external resources.
type Resolver = StateT Cache (ExceptT String IO)


-- | Resolve all references within a JSON document, starting from 'target'.
--
-- === Example
-- > resolveReference "." "schema.json#/components/user"
resolveReference :: Resource -> Reference -> IO (Either String Value)
resolveReference target ref =
    fmap fst <$> resolveReference' target ref HashMap.empty


-- | A variant of 'resolveReference' that allows passing and retrieving a 'Cache'.
--
-- This is useful for batch processing where multiple independent references
-- should share a single cache to avoid repeated retrieval of the same
-- resources.
resolveReference' :: Resource -> Reference -> Cache -> IO (Either String (Value, Cache))
resolveReference' target ref cache = do
    (refTarget, ptr) <- splitRef target ref
    runExceptT (runStateT (getExternal Set.empty [] refTarget ptr) cache)


-- | Resolve a JSON value containing "$ref" fields.
--
-- @resolveRecursive seen trail target root value@ walks @value@ recursively
-- and replaces each object of the form @{ "$ref": <ref> }@ with the JSON value
-- obtained by resolving that reference.  Resolution follows these rules:
--
-- * Relative file references are interpreted with respect to the directory of
--   @target@.  When a new file is loaded, its absolute path becomes the new
--   @target@ and its parsed content becomes the new @root@ for internal
--   references.
--
-- * JSON Pointers (RFC 6901) in fragments (e.g., "#/foo/bar") are used to
--   navigate the target document.
--
-- * Internal References (e.g. @{ "$ref": "#/foo" }@) are resolved against the
--   document currently being processed.
--
-- This resolver performs **deep resolution**: if a reference points to another
-- reference, it will continue following the chain until it reaches a terminal
-- value.
--
-- === Caching and State
-- This function operates within a 'StateT' 'Cache'.  Any resources loaded
-- during resolution are stored in the 'Cache' in their **fully resolved**
-- form.  Subsequent references to the same file will use the cached version.
--
-- === Physical Navigation
-- This resolver navigates the **physical** structure of the document.
-- A pointer like "/a/b" (from fragment "#/a/b") will only resolve if the key
-- "a" literally contains a key "b".  It will NOT "peek" through a $ref at key
-- "a" into another resource to find "b".  To reference nested data in other
-- resources, use explicit paths (e.g., "other.json#/b").
resolveRecursive :: Set (Resource, Pointer) -> [Text] -> Resource -> Value -> Value -> Resolver Value
resolveRecursive seen trail target root = \case
    Object obj -> case KeyMap.lookup "$ref" obj of
      Just (String ref) -> do
        unless (KeyMap.size obj == 1) $
          lift . throwE $ "object contains keys other than '$ref' at " <> showRef target trail

        refKey@(refTarget, ptr) <- lift . lift $ splitRef target ref
        if Set.member refKey seen then
          lift . throwE $ "circular reference detected at " <> Text.unpack ref
        else do
          let !seen' = Set.insert refKey seen
          if refTarget == target
            then getInternal seen' trail refTarget ptr root
            else getExternal seen' trail refTarget ptr
      Just _ ->
        lift . throwE $ "'$ref' is not a string at " <> showRef target trail
      Nothing ->
        Object <$> KeyMap.traverseWithKey (\k -> resolveRecursive seen (Key.toText k : trail) target root) obj
    Array arr ->
      Array <$> Vector.imapM (\i -> resolveRecursive seen (Text.show i : trail) target root) arr
    v ->
      pure v


-- | Resolve a reference that points within the current resource.
--
-- This function is used when the reference target is located in the same file
-- as the source.  It navigates to the requested 'Pointer' within the current
-- 'root' 'Value' and continues the recursive expansion from there.
getInternal :: Set (Resource, Pointer) -> [Text] -> Resource -> Pointer -> Value -> Resolver Value
getInternal seen trail target ptr root = do
    val <- lift . except $ resolvePointer target ptr root
    resolveRecursive seen trail target root val


-- | Resolve a reference that points to a different resource.
--
-- This function handles the transition between resources.  It ensures the
-- 'target' resource is loaded and its entire tree is deeply resolved before
-- caching it. Once the resource is ready, it extracts the requested 'ptr'.
getExternal :: Set (Resource, Pointer) -> [Text] -> Resource -> Pointer -> Resolver Value
getExternal seen trail target ptr = do
    doc <- gets (HashMap.lookup target) >>= \case
      Just cached -> pure cached
      Nothing -> do
        parsed <- lift . ExceptT $ Aeson.eitherDecodeFileStrict target
        resolved <- resolveRecursive seen trail target parsed parsed
        modify' (HashMap.insert target resolved)
        pure resolved
    lift . except $ resolvePointer target ptr doc


-- | Split a reference string into a target resource and a JSON pointer.
--
-- This function handles the URI-like syntax of JSON references:
--
-- 1. If the reference starts with @#@ (e.g., @#/foo@), it returns the
--    current 'target' and the pointer @/foo@.
-- 2. If the reference includes a file path (e.g., @other.json#/foo@),
--    it resolves the path relative to the directory containing the
--    current 'target'.
--
-- The resulting 'Resource' is canonicalized to ensure it can be used reliably
-- as a key in the 'Cache' and for circular reference detection.
splitRef :: Resource -> Reference -> IO (Resource, Pointer)
splitRef target ref = do
    let (base, ptr) = bimap (Text.unpack) (Text.drop 1) $ Text.break (=='#') ref
    refTarget <-
      if null base
        then pure target
        else canonicalizePath $ if isRelative base
                                   then takeDirectory target </> base
                                   else base
    pure (refTarget, ptr)


-- | Navigate a JSON 'Value' using a JSON Pointer (RFC 6901).
resolvePointer :: Resource -> Pointer -> Value -> Either String Value
resolvePointer target ptr root =
    if Text.null ptr then Right root
    else
      case Text.split (=='/') ptr of
        "" : segments -> go [] (unescape . decodeSegment <$> segments) root
        _ -> Left "JSON Pointer must be empty or start with '/'"
  where
    decodeSegment = decodeUtf8 . urlDecode True . encodeUtf8
    unescape = Text.replace "~1" "/"
             . Text.replace "~0" "~"
    go trail = \case
        p : ps -> \case
          Object object ->
            case KeyMap.lookup (Key.fromText p) object of
              Just v -> go (p : trail) ps v
              Nothing -> Left $ "key '" <> Text.unpack p <> "' not found at " <> showRef target trail
          Array array ->
            case Text.Read.decimal p of
              Right (i, "") | i >= 0 && i < Vector.length array ->
                go (p : trail) ps (array Vector.! i)
              _ -> Left $ "invalid array index " <> Text.unpack p <> " at " <> showRef target trail
          _ -> Left $ "cannot descend into non-container value at " <> showRef target trail
        [] -> Right


showRef :: Resource -> [Text] -> String
showRef target trail =
    target <> ('#' : concatMap ('/':) (reverse (Text.unpack <$> trail)))
