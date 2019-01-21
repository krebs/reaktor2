{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.System (plugin) where

import           Blessings
import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Exception (finally)
import           Data.Aeson
import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.Map as M
import           Reaktor.Message
import           Reaktor.Internal
import           Reaktor.Plugins.System.Internal
import           System.Environment (getEnvironment)
import           System.FilePath.Posix (takeBaseName)
import           System.IO (Handle,hClose,hPutStr,hIsEOF)
import           System.IO (BufferMode(LineBuffering),hSetBuffering)
import           System.Process (StdStream(CreatePipe),waitForProcess)
import           System.Process (createProcess,CreateProcess(..),proc)
import qualified Text.Regex.PCRE.Heavy as RE
import qualified Text.Regex.PCRE.Light as RE


plugin :: Value -> IO Plugin
plugin = simplePlugin run


-- TODO indicated whether other plugins should run
run :: SystemConfig -> PluginFunc

run cfg (Message (Just prefix) "PRIVMSG" (msgtarget:text:[])) = do
    nick_ <- getNick
    let hs = maybe [] id (M.lookup "PRIVMSG" (hooks cfg))
    mapM_ (\h -> run1 cfg nick_ h prefix msgtarget text) hs

run cfg (Message (Just prefix) "JOIN" (channel:[])) = do
    nick_ <- getNick
    let hs = maybe [] id (M.lookup "JOIN" (hooks cfg))
    mapM_ (\h -> run1 cfg nick_ h prefix channel "") hs

-- TODO warning?
run _ _ = return ()


run1 ::
    SystemConfig
    -> Nickname
    -> SystemParams
    -> BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
    -> PluginIO ()
run1 cfg nick_ params prefix msgtarget text = do
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
            sendMsg_ <- gets s_sendMsg
            putLog_ <- gets s_putLog
            let onErrLine s =
                  putLog_ $ SGR [31,1] $
                    Plain (BS.pack (takeBaseName $ commandPath c) <> ": "<> s)

                onOutLine s =
                  sendMsg_ (privmsg audience [s])

                extraEnv = [("_prefix", BS.unpack prefix),
                            ("_from", BS.unpack from)]

            lift $ fork cfg c args' (Just extraEnv) "" onOutLine onErrLine

          Nothing -> do
            sendMsg (privmsg audience (resultPrefix <> [cmdName <> ": command not found"]))

      Nothing -> return ()



fork :: SystemConfig
     -> SystemCommand
     -> [String]
     -> Maybe [(String, String)]
     -> String
     -> (BS.ByteString -> IO ())
     -> (BS.ByteString -> IO ())
     -> IO ()
fork cfg cmd args extraEnv input onOutLine onErrLine = do

    baseEnv <- getEnvironment

    let procEnv = M.toList $ mconcat [
                    maybe mempty M.fromList extraEnv,
                    maybe mempty id (commandEnv cmd),
                    M.fromList baseEnv
                  ]

    (inh, outh, errh) <- do
      (Just inh, Just outh, Just errh, ph) <-
        createProcess (proc (commandPath cmd) args) {
          cwd = commandWorkDir cmd <|> defaultWorkDir cfg,
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


hWithLines :: Handle -> (BS.ByteString -> IO ()) -> IO ()
hWithLines h f = do
    hSetBuffering h LineBuffering
    go `finally` hClose h
  where
    go =
      hIsEOF h >>= \case
        True -> return ()
        False -> BS.hGetLine h >>= f >> go
