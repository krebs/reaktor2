{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.SASL where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Base64.Types (extractBase64)
import Data.Text qualified as Text
import Data.Text.Encoding.Base64 (encodeBase64)
import Prelude.Extended
import Reaktor
import Text.Read (readMaybe)


data Mechanism = PLAIN
  deriving (Read, Show)

instance ToJSON Mechanism where
  toJSON = String . Text.show

instance FromJSON Mechanism where
  parseJSON = withText "Mechanism" $ \s ->
    case readMaybe (Text.unpack s) of
      Just x  -> pure x
      Nothing -> fail "Invalid Mechanism value"


data Config = Config
    { cMechanism :: Mechanism
    , cUserName :: Text
    , cPassword :: Text
    }

instance Default Config where
  def = Config
          { cMechanism = PLAIN
          , cUserName = ""
          , cPassword = ""
          }

instance FromJSON Config where
  parseJSON = \case
    Object v ->
      Config
        <$> v .:? "mechanism" .!= PLAIN
        <*> v .: "username"
        <*> v .: "password"
    invalid ->
      typeMismatch "Config" invalid


new :: Config -> Actions -> IO (Message -> IO ())
new Config{..} Actions{..} =
    return $ \case
      Start | cUserName /= "" && cPassword /= "" -> do
        aSend (Message Nothing CAP ["REQ", "sasl"])

      Message _ CAP [_username, "ACK", "sasl"] -> do
        aSend (Message Nothing AUTHENTICATE [Text.show cMechanism])

      Message _ AUTHENTICATE ["+"] ->
        case cMechanism of
          PLAIN ->
            mapM_ (\chunk -> aSend (Message Nothing AUTHENTICATE [chunk]))
                  (toAuthChunks (toBase64 ("\0" <> cUserName <> "\0" <> cPassword)))

      Message _ RPL_SASLSUCCESS _ ->
        aSend (Message Nothing CAP ["END"])

      Message _ ERR_SASLFAIL _ ->
        aSend (Message Nothing CAP ["END"])

      Message _ ERR_SASLTOOLONG _ ->
        aSend (Message Nothing CAP ["END"])

      Message _ ERR_SASLALREADY _ ->
        aSend (Message Nothing CAP ["END"])

      _ -> pure ()


toBase64 :: Text -> Text
toBase64 = extractBase64 . encodeBase64


-- Split input Text into chunks compatibe with the AUTHENTICATE command.
-- This function assumes that each character in the input Text is exactly one
-- byte long, which is true for any Base64 encoded Text.
--
-- See also: https://ircv3.net/specs/extensions/sasl-3.1#the-authenticate-command
toAuthChunks :: Text -> [Text]
toAuthChunks s =
    if not (null chunks) && Text.length (last chunks) == 400 then
      chunks <> ["+"]
    else
      chunks
  where
    chunks = toChunksOf 400 s


toChunksOf :: Int -> Text -> [Text]
toChunksOf n s
    | Text.null s = []
    | otherwise = let (a,b) = Text.splitAt n s
                  in a : toChunksOf n b
