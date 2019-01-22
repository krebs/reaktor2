{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.System (new) where

--import Prelude.Extended
import Blessings
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Exception (finally)
--import Data.Aeson
import Data.ByteString.Char8.Extended (ByteString)
import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.Map as M
import Reaktor
import System.Environment (getEnvironment)
import System.FilePath.Posix (takeBaseName)
import System.IO (BufferMode(LineBuffering),hSetBuffering)
import System.IO (Handle,hClose,hPutStr,hIsEOF)
import Reaktor.Plugins.System.Internal -- TODO rename to Reaktor.Plugins.System again
import System.Process (StdStream(CreatePipe),waitForProcess)
import System.Process (createProcess,CreateProcess(..),proc)
import qualified Text.Regex.PCRE.Heavy as RE
import qualified Text.Regex.PCRE.Light as RE


new :: Config -> Actions -> IO (Message -> IO ())
new config@Config{..} actions@Actions{..} = do
    pure $ \case
        Message (Just prefix) "PRIVMSG" (msgtarget:text:[]) -> do

          nick_ <- aGetNick
          let hs = maybe [] id (M.lookup "PRIVMSG" cHooks)
          mapM_ (\h -> run1 config actions nick_ h prefix msgtarget text) hs

        Message (Just prefix) "JOIN" (channel:[]) -> do
          nick_ <- aGetNick
          let hs = maybe [] id (M.lookup "JOIN" cHooks)
          mapM_ (\h -> run1 config actions nick_ h prefix channel "") hs

        _ -> pure ()


run1 :: Config -> Actions -> ByteString -> SystemParams -> ByteString -> ByteString -> ByteString -> IO ()
run1 config@Config{..} actions@Actions{..} nick_ params prefix msgtarget text = do
    let
        isActivated =
          case activate params of
            Always -> Just ""
            Match ->
              case pattern params of
                Nothing -> Nothing
                Just pat ->
                  let
                      result = RE.scan patternRE text
                      patternRE = RE.compile pat []
                  in
                    if null result
                      then Nothing
                      else Just ""
            Query ->
              if
                | BS.isPrefixOf (nick_ <> ":") text ->
                  Just (nick_ <> ":")
                | BS.isPrefixOf "*:" text ->
                  Just "*:"
                | isQuery ->
                  Just ""
                | otherwise ->
                  Nothing

        audience = if isQuery then from else msgtarget

        -- TODO check if msgtarget is one of our channels?
        --      what if our nick has changed?
        isQuery = msgtarget == nick_

        from = BS.takeWhile (/='!') prefix
          --maybe prefix (flip BS.take prefix) $ BS.findIndex (=='!') prefix

    case isActivated of
      Just trigger -> do
        let cmdline = BS.dropWhile (==' ') $ BS.drop (BS.length trigger) text
            resultPrefix = if isQuery then [] else [from <> ":"]

            parseCommandLine' pat s =
                if null result then [] else snd (head result)
              where
                result = RE.scan patternRE s
                patternRE = RE.compile pat []

            parse' =
              case pattern params of
                Nothing -> [] -- TODO everything
                Just pat -> parseCommandLine' pat cmdline

            headMaybe x = if null x then Nothing else Just (head x)

            -- TODO rename "command" to something like "commandSpec"
            command' = case command params of
              Capture i ->
                case headMaybe (drop (fromIntegral i - 1) parse') of
                  Nothing -> Nothing
                  Just k -> M.lookup k (commands params)

              CaptureOr c -> Just c

            cmdName = case command params of
              Capture i ->
                case headMaybe (drop (fromIntegral i - 1) parse') of
                  Nothing -> "<CMDERP>"
                  Just k -> k

              CaptureOr c -> BS.pack (takeBaseName $ commandPath c)

            args' =
                map BS.unpack $
                map (maybe "" id) $
                reverse $
                dropWhile (==Nothing) $
                reverse $
                map f (arguments params)
              where
                f arg = case arg of
                  Capture i ->
                    case headMaybe (drop (fromIntegral i - 1) parse') of
                      Nothing -> Nothing
                      Just k -> Just k

                  CaptureOr x -> Just x

        case command' of
          Just c -> do
            -- aSend <- gets s_sendMsg
            -- putLog_ <- gets s_putLog
            let onErrLine s =
                  aLog $ SGR [31,1] $
                    Plain (BS.pack (takeBaseName $ commandPath c) <> ": "<> s)

                onOutLine s =
                  aSend (privmsg audience [s])

                extraEnv = [("_prefix", BS.unpack prefix),
                            ("_from", BS.unpack from)]

            fork config actions c args' (Just extraEnv) "" onOutLine onErrLine

          Nothing -> do
            aSend (privmsg audience (resultPrefix <> [cmdName <> ": command not found"]))

      Nothing -> return ()

fork :: Config
     -> Actions
     -> SystemCommand
     -> [String]
     -> Maybe [(String, String)]
     -> String
     -> (ByteString -> IO ())
     -> (ByteString -> IO ())
     -> IO ()
fork Config{..} Actions{..} cmd args extraEnv input onOutLine onErrLine = do

    baseEnv <- getEnvironment

    let procEnv = M.toList $ mconcat [
                    maybe mempty M.fromList extraEnv,
                    maybe mempty id (commandEnv cmd),
                    M.fromList baseEnv
                  ]

    (inh, outh, errh) <- do
      (Just inh, Just outh, Just errh, ph) <-
        createProcess (proc (commandPath cmd) args) {
          cwd = commandWorkDir cmd <|> cDefaultWorkDir,
          env = Just procEnv,
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          close_fds = True,
          create_group = True,
          new_session = True
        }
      _ <- forkIO $ waitForProcess ph >> return ()
      return (inh, outh, errh)

    mapM_ forkIO [
        hPutStr inh input `finally` hClose inh,
        hWithLines outh onOutLine,
        hWithLines errh onErrLine
      ]


hWithLines :: Handle -> (ByteString -> IO ()) -> IO ()
hWithLines h f = do
    hSetBuffering h LineBuffering
    go `finally` hClose h
  where
    go =
      hIsEOF h >>= \case
        True -> return ()
        False -> BS.hGetLine h >>= f >> go
