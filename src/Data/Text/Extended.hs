{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Extended
    ( module Data.Text
    , show
    ) where

import Data.Text
import Prelude hiding (show)
import qualified Prelude

show :: Show a => a -> Text
show = pack . Prelude.show
