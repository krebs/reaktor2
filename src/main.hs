{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Lens
import Data.Aeson
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Text (Text)
import Prelude.Extended
import qualified Reaktor
import qualified Reaktor.Plugins.Mention
import qualified Reaktor.Plugins.Ping
import qualified Reaktor.Plugins.Register
import qualified Reaktor.Plugins.System
import qualified System.Environment


main :: IO ()
main = do
    [configPath] <- System.Environment.getArgs

    v <- preview _Value <$> readFile configPath

    Reaktor.run (reaktorConfig v) (apiConfig v) $ \actions ->
      mapM id [
        Reaktor.Plugins.Mention.new actions,
        Reaktor.Plugins.Ping.new actions,
        Reaktor.Plugins.Register.new (pluginConfig "register" v) actions,
        Reaktor.Plugins.System.new (pluginConfig "system" v) actions
      ]


apiConfig :: (FromJSON b) => Maybe Value -> Maybe b
apiConfig = \case
    Just v -> maybe Nothing parseOrDie (v ^? key "API")
    Nothing -> Nothing

reaktorConfig :: (FromJSON b, Default b) => Maybe Value -> b
reaktorConfig = maybe def parseOrDie

pluginConfig :: (AsValue a, FromJSON b, Default b) => Text -> Maybe a -> b
pluginConfig k v = maybe def parseOrDie (v ^? plugin k)


plugin :: (Applicative f, AsValue a) =>
          Text -> (Value -> f Value) -> Maybe a -> f (Maybe a)
plugin k = _Just
      . key "plugins"
      . values
      . filtered (has (key "plugin" . _String . only k))
      . key "config"


parseOrDie :: FromJSON p => Value -> p
parseOrDie = either error id . parseEither parseJSON
