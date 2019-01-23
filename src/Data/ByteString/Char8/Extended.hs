{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ByteString.Char8.Extended
    ( module Data.ByteString.Char8
    , show
    ) where

import Data.Aeson
import Data.ByteString.Char8
import Data.Text.Encoding (encodeUtf8)
import Prelude hiding (show)
import qualified Prelude

instance FromJSON ByteString where
  parseJSON (String t) = pure (encodeUtf8 t)
  parseJSON _ = pure undefined

instance FromJSONKey ByteString where
  fromJSONKey = FromJSONKeyText encodeUtf8

show :: Show a => a -> ByteString
show = pack . Prelude.show
