{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Internal where

import Prelude.Extended
import Blessings
import Data.Aeson
import Network.Socket as Exports (HostName,ServiceName)
import qualified Data.ByteString.Char8.Extended as BS
import System.IO


data Actions = Actions
    { aIsSecure :: Bool

    , aSend :: Message -> IO ()
    , aLog :: Blessings ByteString -> IO ()

    , aSetNick :: ByteString -> IO ()
    , aGetNick :: IO ByteString
    }


data Config = Config
    { cUseTLS :: Bool
    , cHostName :: HostName
    , cServiceName :: ServiceName
    , cNick :: Maybe ByteString
    , cLogHandle :: Handle
    , cLogTime :: Bool
    }

instance Default Config where
  def = Config False "irc.r" "6667" Nothing stderr True

instance FromJSON Config where
  parseJSON = \case
      Object v -> do
        cServiceName <- v .:? "port" .!= cServiceName def
        cUseTLS <- v .:? "useTLS" .!= (cServiceName == tlsPort)
        cHostName <- v .:? "hostname" .!= cHostName def
        cNick <- v .:? "nick"
        cLogHandle <- pure (cLogHandle def)
        cLogTime <- v .:? "logTime" .!= cLogTime def
        pure Config{..}
      _ -> undefined
    where
      tlsPort :: ServiceName
      tlsPort = "6697"


data Message = Message (Maybe ByteString) ByteString [ByteString] | Start
  deriving Show

formatMessage :: Message -> ByteString
formatMessage = \case
    Message mb_prefix cmd params ->
      maybe "" ((":"<>) . (<>" ")) mb_prefix
          <> cmd
          <> BS.concat (map (" "<>) (init params))
          <> if null params then "" else " :" <> last params
          <> "\r\n"
    x -> error ("cannot format " <> show x)
