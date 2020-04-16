{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Reaktor.API where

import Blessings
import Control.Monad.IO.Class
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Reaktor.Internal
import Reaktor.IRC
import Servant


type API =
    ReqBody '[JSON] Message :> PostAccepted '[JSON] NoContent


api :: Proxy API
api = Proxy


main :: Actions -> IO ()
main Actions{..} = do
    run 7777
  where
    app :: Application
    app = serve api server

    server :: Server API
    server =
        serveTest

    serveTest :: Message -> Handler NoContent
    serveTest = \case
        -- Allowing just private messages to (registered) channels for now.
        msg@(Message Nothing PRIVMSG [msgtarget,_]) | isChannelName msgtarget -> do
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
