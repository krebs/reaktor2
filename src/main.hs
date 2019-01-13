{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Data.Aeson (eitherDecodeFileStrict)
import qualified Reaktor
import qualified System.Environment


main :: IO ()
main = do
    [configPath] <- System.Environment.getArgs
    eitherDecodeFileStrict configPath >>= \case
      Right cfg -> Reaktor.run cfg
      Left err -> error err
