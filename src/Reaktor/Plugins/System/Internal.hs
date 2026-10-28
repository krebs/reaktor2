{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.System.Internal where

import Prelude.Extended
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Reaktor ()
import qualified Reaktor.IRC as IRC
import qualified Data.ByteString as BS
import Data.Array (elems)
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE2.ByteString (Regex)
import qualified Text.Regex.PCRE2.ByteString as RE
import Text.Regex.Base.RegexLike (makeRegexOpts)


-- TODO this needs better names :)
data CaptureOr a = Capture Int | CaptureOr a
  deriving Show -- TODO killme

instance FromJSON a => FromJSON (CaptureOr a) where
  parseJSON o@(Number _) = Capture <$> parseJSON o -- TODO don't parse twice
  parseJSON o = CaptureOr <$> parseJSON o

-- TODO query means via direct privmsg and <nick>:
data Activate = Always | Match | Query
  deriving Show

instance FromJSON Activate where
  parseJSON = \case
      String "always" -> pure Always
      String "match" -> pure Match
      String "query" -> pure Query
      invalid -> typeMismatch "Activate" invalid

data Config = Config
    { cWorkDir :: Maybe FilePath
    , cHooks :: HashMap IRC.Command [Hook]
    }
  deriving Show

instance Default Config where
  def = Config Nothing mempty

instance FromJSON Config where
  parseJSON = \case
      Object v ->
        Config
          <$> v .:? "workdir"
          <*> v .:? "hooks" .!= mempty
      invalid -> typeMismatch "Config" invalid

data Hook = Hook
    { hActivate :: Activate
    , hPattern :: Maybe Regex
    , hCommand :: CaptureOr SystemCommand
    , hArguments :: [CaptureOr Text]
    , hEnv :: Maybe (HashMap String String)
    , hWorkDir :: Maybe FilePath
    , hCommands :: HashMap Text SystemCommand
    , hTimeout :: Maybe Int
    }
  deriving Show

instance FromJSON Hook where
  parseJSON = \case
      Object v ->
        Hook
          <$> v .:? "activate" .!= Query
          <*> (fmap compileRegex <$> v .:? "pattern")
          <*> v .: "command"
          <*> v .:? "arguments" .!= []
          <*> v .:? "env"
          <*> v .:? "workdir"
          <*> v .:? "commands" .!= mempty
          <*> (fmap (*1000000) <$> v .:? "timeoutSec" .!= Just 10)
      invalid -> typeMismatch "Hook" invalid

instance Show Regex where
  show _ = "<Regex>"

compileRegex :: ByteString -> Regex
compileRegex = makeRegexOpts RE.compUTF RE.matchNoUTFCheck

-- | Like pcre-light's match: Nothing on no match or error, otherwise the
-- whole match followed by the captures up to the last one that is set,
-- where unset captures are empty.
matchRegex :: Regex -> ByteString -> Maybe [ByteString]
matchRegex r s =
    case unsafePerformIO (RE.execute r s) of
      Right (Just a) ->
        Just $ map substring $ L.dropWhileEnd ((== RE.unusedOffset) . fst) $ elems a
      _ -> Nothing
  where
    substring (o, l)
      | o == RE.unusedOffset = BS.empty
      | otherwise = BS.take l (BS.drop o s)


data SystemCommand = SystemCommand
    { scPath :: FilePath
    , scWorkDir :: Maybe FilePath
    , scEnv :: Maybe (HashMap String String)
    }
  deriving Show

instance FromJSON SystemCommand where
  parseJSON = \case
      Object v ->
        SystemCommand
          <$> v .: "filename"
          <*> v .:? "workdir"
          <*> v .:? "env"
      invalid -> typeMismatch "SystemCommand" invalid
