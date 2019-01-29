{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor
    ( module Exports
    , privmsg
    , run
    ) where

import Blessings.Text
import Control.Concurrent.Extended
import Control.Exception
import Control.Monad (forM_)
import Data.Attoparsec.Text (feed,parse)
import Data.Attoparsec.Text (IResult(Done,Fail,Partial))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.Char as C
import Data.Function (on)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Extended as T
import qualified Data.Text.IO as T
import Data.Time.Clock.System
import Data.Time.Format
import qualified Network.Simple.TCP as TCP
import qualified Network.Simple.TCP.TLS as TLS
import Network.Socket as Exports (HostName,ServiceName)
import Prelude.Extended
import Reaktor.Internal
import Reaktor.Internal as Exports (Actions(..))
import Reaktor.Internal as Exports (Message(Message,Start))
import Reaktor.IRC as Exports
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
          takeLog2 = showUnprintable <$> takeLog1
          takeLog3 = if logToTTY then takeLog2 else stripSGR <$> takeLog2
          takeLog = takeLog3

      (putInMsg, takeInMsg) <- newChan
      (putOutMsg, takeOutMsg) <- newChan
      (shutdown, awaitShutdown) <- newSemaphore
      (aSetNick,aGetNick) <- newRef =<< maybe Nick.getRandom return cNick

      let actions = Actions{..}
          aIsSecure = cUseTLS
          aLog = putLog
          aSend msg = logMsg msg >> putOutMsg msg

          logMsg msg =
              forM_ (logMsgFilter msg) $ \msg' -> do
                let bs = formatMessage msg'
                aLog $ SGR [38,5,235] "> " <> SGR [35,1] (Plain bs)

      mapM_ (\(s, f) -> installHandler s (Catch f) Nothing) [
          (sigINT, shutdown)
        ]

      plugins <- getPlugins actions

      threads <- mapM (\f -> forkIO $ f `finally` shutdown) [
          receiver actions putInMsg sockRecv,
          logger cLogHandle takeLog,
          pinger aSend,
          sender cSendDelay takeOutMsg sockSend,
          splitter plugins takeInMsg
        ]

      putInMsg Start

      awaitShutdown
      mapM_ killThread threads
      putStrLn ""


logger :: System.IO.Handle -> IO (Blessings Text) -> IO ()
logger h takeLog = forever $ takeLog >>= T.hPutStrLn h . pp


pinger :: (Message -> IO ()) -> IO ()
pinger aSend = forever $ do
    threadDelay time
    aSend (Message Nothing PING ["heartbeat"])
  where
    time = 300 * 1000000

receiver :: Actions -> (Message -> IO ()) -> IO (Maybe ByteString) -> IO ()
receiver Actions{..} putInMsg sockRecv =
    receive ""
  where
    decode :: ByteString -> Text
    decode = T.decodeUtf8With (\_err _c -> Just '?')

    receive :: Text -> IO ()
    receive "" =
        sockRecv >>= \case
          Nothing -> logErr "EOL"
          Just buf -> receive (decode buf)

    receive buf =
        go (parse Parser.message buf)
      where
        go :: IResult Text Message -> IO ()
        go = \case
            Done rest msg -> do
              logMsg msg
              putInMsg msg
              receive rest

            p@(Partial _) ->
              sockRecv >>= \case
                Nothing -> logErr ("EOF with partial " <> Plain (T.show p))
                Just buf' -> go (feed p (decode buf'))

            f@(Fail _i _errorContexts _errMessage) ->
              logErr ("failed to parse message: " <> Plain (T.show f))

    logErr s = aLog $ SGR [31,1] ("! receive: " <> s)

    logMsg msg =
        forM_ (logMsgFilter msg) $ \msg' -> do
          let bs = formatMessage msg'
          aLog $ SGR [38,5,235] "< " <> SGR [38,5,244] (Plain bs)


sender :: Maybe Int -> IO Message -> (ByteString -> IO ()) -> IO ()
sender cSendDelay takeOutMsg sockSend =
    forever send
  where
    send = maybe send0 ((send0 >>) . threadDelay) cSendDelay
    send0 = takeOutMsg >>= sockSend . T.encodeUtf8 . formatMessage

splitter :: [Message -> IO ()] -> IO Message -> IO ()
splitter plugins takeInMsg =
    forever $ do
      msg <- takeInMsg
      mapM_ (\f -> forkIO (f msg)) plugins


logMsgFilter :: Message -> Maybe Message
logMsgFilter = \case
    Message _ PING _ -> Nothing
    Message _ PONG _ -> Nothing
    Message p PRIVMSG ["NickServ",xs] | check -> do
        Just (Message p PRIVMSG ["NickServ",xs'])
      where
        check = elem cmd ["IDENTIFY","REGAIN"] && length ws > 2
        ws = T.words xs
        (cmd:ws') = ws
        (nick:_) = ws'
        xs' = T.unwords [cmd, nick, "<password>"]
    msg -> Just msg


showUnprintable :: Blessings Text -> Blessings Text
showUnprintable =
    fmap' showU
  where
    showU :: Text -> Blessings Text
    showU =
        mconcat
          . map (either Plain (hi . Plain . showLitChars))
          . toEither (not . C.isPrint)

    -- like Blessings' fmap, but don't wrap the Plain case in another Plain
    fmap' :: (Text -> Blessings Text) -> Blessings Text -> Blessings Text
    fmap' f = \case
        Append t1 t2 -> Append (fmap' f t1) (fmap' f t2)
        Plain s -> f s
        SGR pm t -> SGR pm (fmap' f t)
        Empty -> Empty

    hi = SGR [38,5,79]

    showLitChars :: Text -> Text
    showLitChars = T.concatMap (T.pack . flip C.showLitChar "")

    toEither :: (Char -> Bool) -> Text -> [Either Text Text]
    toEither p =
      map (\s -> if p (T.head s) then Right s else Left s)
      . T.groupBy ((==) `on` p)


privmsg :: Text -> [Text] -> Message
privmsg msgtarget xs =
    Message Nothing PRIVMSG (msgtarget:T.intercalate " " xs:[])


prefixTimestamp :: Blessings Text -> IO (Blessings Text)
prefixTimestamp s = do
    t <- SGR [38,5,239] . Plain . T.pack <$> getTimestamp
    return (t <> " " <> s)


getTimestamp :: IO String
getTimestamp =
    formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")
    . systemToUTCTime <$> getSystemTime
