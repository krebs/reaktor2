{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor (run) where

import           Blessings (Blessings(Append,Empty,Plain,SGR),pp)
import           Control.Arrow
import           Control.Concurrent (forkIO,killThread,threadDelay)
import           Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import           Control.Exception (finally)
import           Control.Monad (foldM,forever,unless)
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8 (IResult(Done,Fail,Partial))
import           Data.Attoparsec.ByteString.Char8 (feed,parse)
import qualified Data.ByteString.Char8.Extended as BS
import           Data.Foldable (toList)
import qualified Data.Text as T
import           Data.Time.Clock.System
import           Data.Time.Format
import qualified Network.Simple.TCP as TCP
import qualified Network.Simple.TCP.TLS as TLS
import           Reaktor.Config
import           Reaktor.Parser (message)
import qualified Reaktor.Plugins
import           Reaktor.Types
import           System.IO (BufferMode(LineBuffering),hSetBuffering)
import           System.IO (Handle)
import           System.IO (hIsTerminalDevice)
import           System.IO (hPutStr,hPutStrLn,stderr)
import           System.Posix.Signals


run :: Config -> IO ()
run cfg0 = do

    let logh = stderr

    let cfg1 = addPlugin "ping" (Reaktor.Plugins.get "ping" Null) cfg0

    cfg <- initPlugins cfg1

    let tlsPlugins =
          T.unpack $
          T.intercalate ", " $
          map pi_name $
          filter (requireTLS . either undefined id . pi_plugin)
                 (pluginInstances cfg)

    unless (useTLS cfg || null tlsPlugins) $ do
      error $ "Not using TLS, but following plugins require it: " <> tlsPlugins

    -- TODO reset when done?
    hSetBuffering logh LineBuffering
    logToTTY <- hIsTerminalDevice logh
    let logFilter = if logToTTY then id else stripSGR

    connect cfg $ \send_ recv_ -> do
      (putLog, takeLog) <- newRelay
      (putMsg, takeMsg) <- newRelay
      (shutdown, awaitShutdown) <- newSemaphore

      mapM_ (\(s, f) -> installHandler s (Catch f) Nothing) [
          (sigINT, shutdown)
        ]

      let prefixTimestamp s = do
              t <- SGR [38,5,239] . Plain . BS.pack <$> getTimestamp
              return (t <> " " <> s)

          takeLog' =
            if logTime cfg
              then takeLog >>= prefixTimestamp
              else takeLog

      threadIds <- mapM (\f -> forkIO $ f `finally` shutdown) [
          driver cfg putLog putMsg recv_,
          logger logFilter takeLog' logh,
          pinger putLog putMsg,
          sender takeMsg send_
        ]

      awaitShutdown
      mapM_ killThread threadIds
      hPutStrLn logh ""
  where

    pinger :: (Blessings BS.ByteString -> IO ()) -> (Message -> IO ()) -> IO ()
    pinger putLog putMsg = forever $ do
        threadDelay time
        sendIO putLog putMsg (Message Nothing "PING" ["heartbeat"])
      where
        time = 300 * 1000000

    sender :: IO Message -> (BS.ByteString -> IO ()) -> IO ()
    sender takeMsg send_ =
        forever $ takeMsg >>= send_ . formatMessage

    logger :: (Blessings BS.ByteString -> Blessings BS.ByteString)
           -> IO (Blessings BS.ByteString)
           -> Handle
           -> IO ()
    logger f takeLog h = forever $ do
        s <- takeLog
        let s' = if lastChar s == '\n' then s else s <> Plain "\n"
        hPutStr h $ pp $ fmap BS.unpack (f s')
      where
        lastChar :: Blessings BS.ByteString -> Char
        lastChar = BS.last . last . toList

    stripSGR :: Blessings a -> Blessings a
    stripSGR = \case
        Append t1 t2 -> Append (stripSGR t1) (stripSGR t2)
        SGR _ t -> stripSGR t
        Plain x -> Plain x
        Empty -> Empty


connect :: Config
        -> ((BS.ByteString -> IO ()) -> IO (Maybe BS.ByteString) -> IO ())
        -> IO ()
connect cfg action = do
    if useTLS cfg then do
      s <- TLS.getDefaultClientSettings (hostname cfg, BS.pack (port cfg))
      TLS.connect s (hostname cfg) (port cfg) $ \(ctx, _sockAddr) -> do
        let send = TLS.send ctx
            recv = TLS.recv ctx
        action send recv
    else do
      TCP.connect (hostname cfg) (port cfg) $ \(sock, _sockAddr) -> do
        let send = TCP.send sock
            recv = TCP.recv sock 512
        action send recv

driver :: Config
          -> (Blessings BS.ByteString -> IO ())
          -> (Message -> IO ())
          -> IO (Maybe BS.ByteString)
          -> IO ()

driver cfg putLog putMsg recv_ = do
    cfg' <- handleMessage cfg putMsg putLog (Message Nothing "<start>" [])
    drive cfg' putMsg putLog recv_ ""

drive :: Config
      -> (Message -> IO ())
      -> (Blessings BS.ByteString -> IO ())
      -> IO (Maybe BS.ByteString)
      -> BS.ByteString
      -> IO ()
drive cfg putMsg putLog recv_ "" =
    recv_ >>= \case
      Nothing -> putLog $ SGR [34,1] (Plain "# EOL")
      Just msg -> drive cfg putMsg putLog recv_ msg

drive cfg putMsg putLog recv_ buf =
    go (parse message buf)
  where
    go :: IResult BS.ByteString Message -> IO ()
    go = \case
        Done rest msg -> do
          -- TODO log message only if h hasn't disabled logging for it
          let s = formatMessage msg
          putLog $ SGR [38,5,235] "< " <> SGR [38,5,244] (Plain s)
          cfg' <- handleMessage cfg putMsg putLog msg
          drive cfg' putMsg putLog recv_ rest

        p@(Partial _) -> do
          recv_ >>= \case
            Nothing -> do
              putLog $ SGR [34,1] (Plain "# EOL")
            Just msg ->
              go (feed p msg)

        f@(Fail _i _errorContexts _errMessage) ->
          putLog $ SGR [31,1] (Plain (BS.pack $ show f))

handleMessage :: Config
              -> (Message -> IO ())
              -> (Blessings BS.ByteString -> IO ())
              -> Message
              -> IO Config
handleMessage cfg putMsg putLog msg = do
    let
        q0 = PluginState {
              s_putLog = putLog,
              s_nick = nick cfg,
              s_sendMsg = sendIO putLog putMsg,
              s_sendMsg' = sendIO' putLog putMsg
            }

        f q i =
          execStateT (pluginFunc (either undefined id (pi_plugin i)) msg) q

    q' <- foldM f q0 (pluginInstances cfg)

    return cfg { nick = s_nick q' }


formatMessage :: Message -> BS.ByteString
formatMessage (Message mb_prefix cmd params) =
    maybe "" (\x -> ":" <> x <> " ") mb_prefix
        <> cmd
        <> BS.concat (map (" "<>) (init params))
        <> if null params then "" else " :" <> last params
        <> "\r\n"


getTimestamp :: IO String
getTimestamp =
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")
    . systemToUTCTime <$> getSystemTime


newRelay :: IO (a -> IO (), IO a)
newRelay = (putMVar &&& takeMVar) <$> newEmptyMVar


newSemaphore :: IO (IO (), IO ())
newSemaphore = first ($()) <$> newRelay


sendIO :: (Blessings BS.ByteString -> IO ())
       -> (Message -> IO ())
       -> Message
       -> IO ()
sendIO putLog putMsg msg =
    sendIO' putLog putMsg msg msg

sendIO' :: (Blessings BS.ByteString -> IO ())
       -> (Message -> IO ())
       -> Message
       -> Message
       -> IO ()
sendIO' putLog putMsg msg logMsg = do
    putLog $ SGR [38,5,235] "> " <> SGR [35,1] (Plain $ formatMessage logMsg)
    putMsg msg
