{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.System (new) where

import Blessings
import Control.Applicative
import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception
import Control.Monad (forM_)
import qualified Data.HashMap.Lazy as M
import qualified Data.List as L
import qualified Data.Text.Extended as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Prelude.Extended
import Reaktor
import Reaktor.Plugins.System.Internal
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath.Posix (takeBaseName)
import System.IO (BufferMode(LineBuffering),hSetBuffering)
import System.IO (Handle,hClose,hPutStr,hIsEOF)
import System.IO.Error (catchIOError,isDoesNotExistError)
import System.Process
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals (Signal,signalProcessGroup,killProcess)
import System.Posix.Types (ProcessGroupID)
import qualified Text.Regex.PCRE.Light as RE


new :: Config -> Actions -> IO (Message -> IO ())
new config@Config{..} actions@Actions{..} = do
    pure $ \case
        Message (Just prefix) PRIVMSG (msgtarget:text:[]) -> do
          let hooks = maybe [] id (M.lookup PRIVMSG cHooks)
          mapM_ (\h -> run1 config actions h prefix msgtarget text) hooks

        Message (Just prefix) JOIN (channel:[]) -> do
          let hooks = maybe [] id (M.lookup JOIN cHooks)
          mapM_ (\h -> run1 config actions h prefix channel "") hooks

        _ -> pure ()


run1 :: Config -> Actions -> Hook -> Text -> Text -> Text -> IO ()
run1 Config{..} Actions{..} Hook{..} prefix msgtarget text = do
    nick <- aGetNick

    let
        match =
          case hPattern of
            Just p -> \s ->
              fmap (map T.decodeUtf8) $
              RE.match p (T.encodeUtf8 s) [RE.exec_no_utf8_check]
            Nothing -> const Nothing

        isActivated =
          case hActivate of
            Always -> pure ""
            Match -> match text >> pure ""
            Query ->
              let me = nick <> ":" in
              if
                | isQuery -> pure ""
                | T.isPrefixOf me text -> pure me
                | T.isPrefixOf "*:" text -> pure "*:"
                | otherwise -> mempty

        audience = if isQuery then from else msgtarget

        from = T.takeWhile (/='!') prefix

        -- TODO check if msgtarget is one of our channels?
        --      what if our nick has changed?
        isQuery = msgtarget == nick

    forM_ isActivated $ \activationPrefix -> do
      let
          text' = T.drop (T.length activationPrefix) text
          resultPrefix = if isQuery then [] else [from <> ":"]

          captures = V.fromList $ fromMaybe [] (match text')
          capture i = captures V.!? i

          name =
            case hCommand of
              Capture i -> fromMaybe "<unnamed>" (capture i)
              CaptureOr SystemCommand{..} -> T.pack $ takeBaseName $ scPath

          command =
            case hCommand of
              Capture i -> (`M.lookup` hCommands) =<< capture i
              CaptureOr c -> Just c

          args =
            map (maybe "" T.unpack)
              $ L.dropWhileEnd isNothing
              $ flip map hArguments
              $ \case
                Capture i -> capture i
                CaptureOr s -> Just s

      case command of
        Just SystemCommand{..} -> do
          baseEnv <- getEnvironment

          let
              logStr pid s = do
                let p = name <> "[" <> T.show pid <> "] "
                aLog $ SGR [38,5,247] (Plain p <> s)

              red :: Text -> Blessings Text
              red = SGR [31] . Plain

              onStart pid = logStr pid "started"
              onErrLine pid s = logStr pid $ "stderr: " <> red s
              onOutLine _ s = aSend (privmsg audience [s])
              onError pid e = logStr pid $ "failed: " <> red (T.show e)

              onExit pid = \case
                ExitSuccess ->
                  logStr pid "stopped"
                ExitFailure i ->
                  logStr pid $ "stopped with exit code " <>
                    red (T.show $ if i <= 127 then i else -256 + i)

              extraEnv =
                  [ ("_prefix", T.unpack prefix)
                  , ("_from", T.unpack from)
                  , ("_msgtarget", T.unpack msgtarget)
                  ]

              env =
                M.toList $ mconcat
                  [ M.fromList extraEnv
                  , maybe mempty id scEnv
                  , M.fromList baseEnv
                  ]

              cwd = scWorkDir <|> hWorkDir <|> cWorkDir

          fork scPath args cwd (Just env) "" hTimeout Callbacks{..}

        Nothing -> do
          let s = name <> ": command not found"
          aSend (privmsg audience (resultPrefix <> [s]))


data Callbacks = Callbacks
    { onOutLine :: Pid -> Text -> IO ()
    , onErrLine :: Pid -> Text -> IO ()
    , onError :: Pid -> SomeException -> IO ()
    , onExit :: Pid -> ExitCode -> IO ()
    , onStart :: Pid -> IO ()
    }

fork :: FilePath
     -> [String]
     -> Maybe FilePath
     -> Maybe [(String, String)]
     -> String
     -> Maybe Int
     -> Callbacks
     -> IO ()
fork path args cwd env input hTimeout Callbacks{..} =
    forkIO (f `catch` onError (-1)) >> return ()
  where
    f = withCreateProcess p $ \(Just inh) (Just outh) (Just errh) ph -> do
      Just pid <- getPid ph
      pgid <- getProcessGroupIDOf pid

      onStart pid

      mapM_ forkIO [
          hPutStr inh input `finally` hClose inh,
          hWithLines outh (onOutLine pid),
          hWithLines errh (onErrLine pid)
        ]

      case hTimeout of
        Just time ->
          race (threadDelay time) (waitForProcess ph) >>= \case
            Left () -> onError pid (SomeException (ErrorCall "timeout"))
            Right code -> onExit pid code
        Nothing ->
          waitForProcess ph >>= onExit pid

      killProcessGroup pgid

    p = (proc path args)
          { cwd = cwd
          , env = env
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , close_fds = True
          , create_group = True
          , new_session = True
          }


killProcessGroup :: ProcessGroupID -> IO ()
killProcessGroup = signalProcessGroup' killProcess

signalProcessGroup' :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup' sig pgid =
    catchIOError
      (signalProcessGroup sig pgid)
      (\e -> if isDoesNotExistError e then return () else ioError e)


hWithLines :: Handle -> (Text -> IO ()) -> IO ()
hWithLines h f = do
    hSetBuffering h LineBuffering
    go `finally` hClose h
  where
    go =
      hIsEOF h >>= \case
        True -> return ()
        False -> T.hGetLine h >>= f >> go
