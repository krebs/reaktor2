{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ByteString.Char8.Extended
    ( module Data.ByteString.Char8
    ) where

import Data.Aeson
import Data.ByteString.Char8
import Data.Text.Encoding (encodeUtf8)

instance FromJSON ByteString where
  parseJSON (String t) = pure (encodeUtf8 t)
  parseJSON _ = pure undefined

instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8
