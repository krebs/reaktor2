{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor
    ( module Exports
    , privmsg
    , run
    ) where

import Blessings
import Control.Concurrent.Extended
import Control.Exception
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (toList)
import Data.Time.Clock.System
import Data.Time.Format
import qualified Network.Simple.TCP as TCP
import qualified Network.Simple.TCP.TLS as TLS
import Network.Socket as Exports (HostName,ServiceName)
import Prelude.Extended
import Reaktor.Internal
import Reaktor.Internal as Exports (Actions(..))
import Reaktor.Internal as Exports (Message(Message,Start))
import Reaktor.Internal as Exports (formatMessage)
import Reaktor.Nick as Exports
import Reaktor.Nick as Nick
import qualified Reaktor.Parser as Parser
import qualified System.IO
import System.IO (BufferMode(LineBuffering),hSetBuffering)
import System.IO (hIsTerminalDevice)
import System.Posix.Signals


run :: Config -> (Actions -> IO [Message -> IO ()]) -> IO ()
run Config{..} getPlugins =
    if cUseTLS then do
      s <- TLS.getDefaultClientSettings (cHostName, BS.pack cServiceName)
      TLS.connect s cHostName cServiceName $ \(ctx, sockAddr) ->
        withSocket sockAddr (TLS.send ctx) (TLS.recv ctx)
    else do
      TCP.connect cHostName cServiceName $ \(sock, sockAddr) ->
        withSocket sockAddr (TCP.send sock) (TCP.recv sock 512)
  where
    withSocket _sockAddr sockSend sockRecv = do

      hSetBuffering cLogHandle LineBuffering -- TODO reset
      logToTTY <- hIsTerminalDevice cLogHandle
      (putLog, takeLog0) <- newChan
      let
          takeLog1 = if cLogTime then takeLog0 >>= prefixTimestamp else takeLog0
          takeLog2 = if logToTTY then takeLog1 else stripSGR <$> takeLog1
          takeLog = takeLog2

      (putInMsg, takeInMsg) <- newChan
      (putOutMsg, takeOutMsg) <- newChan
      (shutdown, awaitShutdown) <- newSemaphore
      (aSetNick,aGetNick) <- newRef =<< maybe Nick.getRandom return cNick

      let actions = Actions{..}
          aIsSecure = cUseTLS
          aLog = putLog
          aLogMsg msg = do
              let bs = formatMessage msg
              putLog $ SGR [38,5,235] "> " <> SGR [35,1] (Plain bs)
          aSendQuiet = putOutMsg
          aSend msg = aLogMsg msg >> aSendQuiet msg

      mapM_ (\(s, f) -> installHandler s (Catch f) Nothing) [
          (sigINT, shutdown)
        ]

      plugins <- getPlugins actions

      threads <- mapM (\f -> forkIO $ f `finally` shutdown) [
          receiver actions putInMsg sockRecv,
          logger cLogHandle takeLog,
          pinger aSend,
          sender takeOutMsg sockSend,
          splitter plugins takeInMsg
        ]

      putInMsg Start

      awaitShutdown
      mapM_ killThread threads
      putStrLn ""


logger :: System.IO.Handle -> IO (Blessings ByteString) -> IO ()
logger h takeLog = forever $ do
    s <- takeLog
    let s' = if lastChar s == '\n' then s else s <> Plain "\n"
    System.IO.hPutStr h $ pp $ fmap BS.unpack s'

pinger :: (Message -> IO ()) -> IO ()
pinger aSend = forever $ do
    threadDelay time
    aSend (Message Nothing "PING" ["heartbeat"])
  where
    time = 300 * 1000000

receiver :: Actions -> (Message -> IO ()) -> IO (Maybe ByteString) -> IO ()
receiver Actions{..} putInMsg sockRecv =
    receive ""
  where
    receive "" =
        sockRecv >>= \case
          Nothing -> do
            aLog $ SGR [34,1] (Plain "# EOL")
          Just buf -> receive buf

    receive buf =
        go (parse Parser.message buf)
      where
        go :: IResult ByteString Message -> IO ()
        go = \case
            Done rest msg -> do
              -- TODO log message only if h hasn't disabled logging for it
              let bs = formatMessage msg
              aLog $ SGR [38,5,235] "< " <> SGR [38,5,244] (Plain bs)
              putInMsg msg
              receive rest

            p@(Partial _) -> do
              sockRecv >>= \case
                Nothing -> do
                  aLog $ SGR [31] (Plain "EOF")
                Just msg ->
                  go (feed p msg)

            f@(Fail _i _errorContexts _errMessage) -> do
              aLog $ SGR [31,1] (Plain (BS.pack $ show f))

sender :: IO Message -> (ByteString -> IO ()) -> IO ()
sender takeOutMsg sockSend =
    forever $ takeOutMsg >>= sockSend . formatMessage

splitter :: [Message -> IO ()] -> IO Message -> IO ()
splitter plugins takeInMsg =
    forever $ do
      msg <- takeInMsg
      mapM_ (\f -> forkIO (f msg)) plugins


privmsg :: ByteString -> [ByteString] -> Message
privmsg msgtarget xs =
    Message Nothing "PRIVMSG" (msgtarget:BS.intercalate " " xs:[])


lastChar :: Blessings ByteString -> Char
lastChar = BS.last . last . toList

prefixTimestamp :: Blessings ByteString -> IO (Blessings ByteString)
prefixTimestamp s = do
    t <- SGR [38,5,239] . Plain . BS.pack <$> getTimestamp
    return (t <> " " <> s)

stripSGR :: Blessings a -> Blessings a
stripSGR = \case
    Append t1 t2 -> Append (stripSGR t1) (stripSGR t2)
    SGR _ t -> stripSGR t
    Plain x -> Plain x
    Empty -> Empty


getTimestamp :: IO String
getTimestamp =
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")
    . systemToUTCTime <$> getSystemTime
