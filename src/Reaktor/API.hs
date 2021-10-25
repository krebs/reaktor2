{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Reaktor.API where

import Blessings
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Function ((&))
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Network.Socket.Extended
import Network.Wai
import Network.Wai.Handler.Warp
import Reaktor.Internal (Actions(..),Message(..))
import Reaktor.IRC
import Servant


type API =
    ReqBody '[JSON] Message :> PostAccepted '[JSON] NoContent


data Config = Config
    { cListen :: String
    }
instance FromJSON Config where
  parseJSON = \case
    Object v -> do
      cListen <- v .: "listen"
      pure Config{..}
    invalid -> typeMismatch "Config" invalid


api :: Proxy API
api = Proxy


main :: Actions -> Maybe Config -> IO ()
main Actions{..} = \case
    Just Config{..} ->
      either disable enable =<< readListenString cListen
    Nothing ->
      disable "no configuration"
  where
    enable sockAddr =
        bracket
          (openSocket sockAddr)
          closeSocket
          $ \sock -> do
            aLog $ SGR [38,5,155]
              ("* enabling API on " <> Plain (T.pack $ show sockAddr))
            let port = getAddrPort sockAddr
                settings = defaultSettings & setPort port
            bind sock sockAddr
            listen sock maxListenQueue
            runSettingsSocket settings sock
              $ app

    disable :: String -> IO ()
    disable reason = do
        aLog $ SGR [38,5,196]
          ("! disabling API due to " <> Plain (T.pack reason))
        forever $ threadDelay 60000

    app :: Application
    app = serve api server

    server :: Server API
    server =
        serveTest

    serveTest :: Message -> Handler NoContent
    serveTest = \case
        msg@(Message Nothing NOTICE [msgtarget,_]) | isChannelName msgtarget -> do
            liftIO $ aSend msg
            return NoContent
        msg@(Message Nothing PRIVMSG [msgtarget,_]) | isChannelName msgtarget -> do
            liftIO $ aSend msg
            return NoContent
        msg@(Message Nothing TOPIC [msgtarget,_]) | isChannelName msgtarget -> do
            liftIO $ aSend msg
            return NoContent
        _ ->
            throwError err403
      where
        -- Channel names are defined in RFC 2812, 1.3
        isChannelName msgtarget =
            case T.uncons msgtarget of
                Just (c, _) -> c `elem` ("&#+!" :: String)
                Nothing -> False
