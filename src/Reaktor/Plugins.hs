{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins (get,registry) where

import           Data.Aeson (Value)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Reaktor.Plugins.Mention
import qualified Reaktor.Plugins.NickServ
import qualified Reaktor.Plugins.Ping
import qualified Reaktor.Plugins.Register
import qualified Reaktor.Plugins.System
import           Reaktor.Types (Plugin)


get :: T.Text -> Value -> IO Plugin
get name =
    case M.lookup name registry of
      Just p -> p
      Nothing -> error ("unknown plugin: " <> T.unpack name)

registry :: M.Map T.Text (Value -> IO Plugin)
registry = M.fromList [
    ("mention", Reaktor.Plugins.Mention.plugin),
    ("NickServ", Reaktor.Plugins.NickServ.plugin),
    ("ping", Reaktor.Plugins.Ping.plugin),
    ("register", Reaktor.Plugins.Register.plugin),
    ("system", Reaktor.Plugins.System.plugin)
  ]
