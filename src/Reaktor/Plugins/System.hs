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
import qualified Data.HashMap.Lazy as M
import qualified Data.List as L
import qualified Data.Text.Extended as T
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
import qualified Text.Regex.PCRE.Heavy as RE
import qualified Text.Regex.PCRE.Light as RE


new :: Config -> Actions -> IO (Message -> IO ())
new config@Config{..} actions@Actions{..} = do
    pure $ \case
        Message (Just prefix) cmd (msgtarget:text:[]) | elem cmd ["PRIVMSG", "JOIN"] -> do
          let hooks = maybe [] id (M.lookup cmd cHooks)
          mapM_ (\h -> run1 config actions h prefix msgtarget text) hooks

        Message (Just prefix) "JOIN" (channel:[]) -> do
          let hooks = maybe [] id (M.lookup "JOIN" cHooks)
          mapM_ (\h -> run1 config actions h prefix channel "") hooks

        _ -> pure ()


run1 :: Config -> Actions -> Hook -> Text -> Text -> Text -> IO ()
run1 Config{..} Actions{..} Hook{..} prefix msgtarget text = do
    nick <- aGetNick

    let
        isActivated =
          case hActivate of
            Always -> Just ""
            Match ->
              case hPattern of
                Nothing -> Nothing
                Just pat ->
                  let
                      result = RE.scan patternRE text
                      patternRE = RE.compile pat [RE.utf8]
                  in
                    if null result
                      then Nothing
                      else Just ""
            Query ->
              if
                | T.isPrefixOf (nick <> ":") text ->
                  Just (nick <> ":")
                | T.isPrefixOf "*:" text ->
                  Just "*:"
                | isQuery ->
                  Just ""
                | otherwise ->
                  Nothing

        audience = if isQuery then from else msgtarget

        from = T.takeWhile (/='!') prefix

        -- TODO check if msgtarget is one of our channels?
        --      what if our nick has changed?
        isQuery = msgtarget == nick

    case isActivated of
      Just trigger -> do
        let
            cmdline = T.dropWhile (==' ') $ T.drop (T.length trigger) text
            resultPrefix = if isQuery then [] else [from <> ":"]

            parseCommandLine' pat s =
                if null result then [] else snd (head result)
              where
                result = RE.scan patternRE s
                patternRE = RE.compile pat [RE.utf8]

            captures =
              V.fromList $
                case hPattern of
                  Nothing -> [] -- TODO everything?
                  Just pat -> parseCommandLine' pat cmdline

            capture i = captures V.!? (i - 1)

            name =
              case hCommand of
                Capture i -> fromMaybe "<unnamed>" (capture i)
                CaptureOr Command{..} -> T.pack $ takeBaseName $ commandPath

            command =
              case hCommand of
                Capture i -> (`M.lookup` hCommands) =<< capture i
                CaptureOr c -> Just c

            args =
              map (maybe "" T.unpack)
                $ L.dropWhileEnd isNothing
                -- $ map getArgument hArguments
                $ flip map hArguments
                $ \case
                  Capture i -> capture i
                  CaptureOr s -> Just s

        case command of
          Just Command{..} -> do
            baseEnv <- getEnvironment

            let
                onExit code = do
                  let s = T.show code
                      (sig, col) =
                        if code == ExitSuccess
                          then (SGR [38,5,235] "* ", SGR [38,5,107])
                          else (SGR [38,5,235] "! ", SGR [31,1])
                  aLog $ sig <> col (Plain $ name <> ": " <> s)

                onExcept :: SomeException -> IO ()
                onExcept e = do
                  let s0 = T.show e
                  aLog $ SGR [38,5,235] "! "
                      <> SGR [31,1] (Plain $ name <> ": " <> s0)

                onErrLine :: Text -> IO ()
                onErrLine s0 = do
                  aLog $ SGR [38,5,235] "2 "
                      <> SGR [31,1] (Plain $ name <> ": " <> s0)

                onOutLine s = aSend (privmsg audience [s])

                extraEnv =
                    [ ("_prefix", T.unpack prefix)
                    , ("_from", T.unpack from)
                    ]

                env =
                  M.toList $ mconcat
                    [ M.fromList extraEnv
                    , maybe mempty id commandEnv
                    , M.fromList baseEnv
                    ]

                cwd = commandWorkDir <|> hWorkDir <|> cWorkDir

            fork commandPath args cwd (Just env) "" hTimeout Callbacks{..}

          Nothing -> do
            let s = name <> ": command not found"
            aSend (privmsg audience (resultPrefix <> [s]))

      Nothing -> return ()

data Callbacks = Callbacks
    { onOutLine :: Text -> IO ()
    , onErrLine :: Text -> IO ()
    , onExit :: ExitCode -> IO ()
    , onExcept :: SomeException -> IO ()
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
    forkIO (f `catch` onExcept) >> return ()
  where
    f = withCreateProcess p $ \(Just inh) (Just outh) (Just errh) ph -> do
      Just pid <- getPid ph
      pgid <- getProcessGroupIDOf pid

      mapM_ forkIO [
          hPutStr inh input `finally` hClose inh,
          hWithLines outh onOutLine,
          hWithLines errh onErrLine
        ]

      case hTimeout of
        Just time ->
          race (threadDelay time) (waitForProcess ph) >>= \case
            Left () -> onExcept (SomeException (ErrorCall "timeout"))
            Right code -> onExit code
        Nothing ->
          waitForProcess ph >>= onExit

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
