{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Config where

import           Data.Aeson
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import           Reaktor.Internal
import qualified Reaktor.Plugins


instance FromJSON Config where
  parseJSON (Object v) = do
      p <- v .:? "port" .!= defaultPort

      Config
        <$> v .: "hostname"
        <*> pure p
        <*> v .: "nick"
        <*> v .:? "useTLS" .!= (p == tlsPort)
        <*> v .:? "logTime" .!= True
        <*> v .:? "plugins" .!= []
  parseJSON _ = pure undefined


data Config = Config {
      hostname :: HostName,
      port :: ServiceName,
      nick :: Nickname,
      useTLS :: Bool,
      logTime :: Bool,
      pluginInstances :: [PluginInstance]
    }


addPlugin :: T.Text -> IO Plugin -> Config -> Config
addPlugin name p r =
    r { pluginInstances = pluginInstances r <> [PluginInstance name (Left p)] }


defaultPort :: ServiceName
defaultPort = tlsPort

tlsPort :: ServiceName
tlsPort = "6697"


data PluginInstance = PluginInstance {
      pi_name :: T.Text,
      pi_plugin :: Either (IO Plugin) Plugin
    }

instance FromJSON PluginInstance where
  parseJSON o@(Object v) =
      case HML.lookup "plugin" v of
        Just (String name) -> do
          let p = Reaktor.Plugins.get name
              c = HML.lookupDefault (Object HML.empty) "config" v
          pure $ PluginInstance name (Left $ p c)
        Just _ -> error ("bad plugin object: " <> show o)
        _ -> error ("mising 'plugin' attribute: " <> show o)
  parseJSON x =
      error ("bad plugin type: " <> show x)


initPlugins :: Config -> IO Config
initPlugins cfg = do
    plugins' <- mapM initPlugin (pluginInstances cfg)
    return cfg { pluginInstances = plugins' }
  where
    initPlugin :: PluginInstance -> IO PluginInstance
    initPlugin i = do
      p <-
        case pi_plugin i of
          Right p -> return p
          Left f -> f
      return i { pi_plugin = Right p }
