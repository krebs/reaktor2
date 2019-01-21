{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.Ping (plugin) where

import           Control.Monad (when)
import           Data.Aeson (Value(Null))
import           Reaktor.Internal


plugin :: Value -> IO Plugin
plugin = simplePlugin (\Null -> run)

run :: PluginFunc
run (Message _ ircCommand args) =
    when (ircCommand == "PING") $
      sendMsg (Message Nothing "PONG" args)
