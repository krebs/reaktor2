{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Internal where

import Prelude.Extended
import Blessings
import Data.Aeson
import Data.String.Conversions (convertString)
import qualified Data.Text as T
import Network.Socket as Exports (HostName,ServiceName)
import Reaktor.IRC
import System.IO


data Actions = Actions
    { aIsSecure :: Bool

    , aSend :: Message -> IO ()
    , aLog :: Blessings Text -> IO ()

    , aSetNick :: Text -> IO ()
    , aGetNick :: IO Text
    }


data Config = Config
    { cUseTLS :: Bool
    , cHostName :: HostName
    , cServiceName :: ServiceName
    , cNick :: Maybe Text
    , cLogHandle :: Handle
    , cLogTime :: Bool
    }
  deriving Show

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

data Message = Message (Maybe Text) Command [Text] | Start
  deriving Show

formatMessage :: Message -> Text
formatMessage = \case
    Message mb_prefix cmd params ->
      maybe "" ((":"<>) . (<>" ")) mb_prefix
          <> convertString cmd
          <> T.concat (map (" "<>) (init params))
          <> if null params then "" else " :" <> last params
          <> "\r\n"
    x -> error ("cannot format " <> show x)
