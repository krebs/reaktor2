{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Types (module Reaktor.Types, module X) where

import           Blessings (Blessings)
import           Control.Monad.Trans.Class as X (lift)
import           Control.Monad.Trans.State as X (gets,modify)
import           Control.Monad.Trans.State (StateT)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8.Extended as BS
import           Network.Socket as X (HostName,ServiceName)


type Prefix = BS.ByteString

type Nickname = BS.ByteString
type Password = BS.ByteString
type MsgTarget = BS.ByteString
type Channel = MsgTarget

data PluginState = PluginState {
      s_putLog :: Blessings BS.ByteString -> IO (),
      s_nick :: BS.ByteString,
      s_sendMsg :: Message -> IO (),
      s_sendMsg' :: Message -> Message -> IO ()
    }

setNick :: Nickname -> PluginIO ()
setNick newnick = modify (\q -> q { s_nick = newnick })

getNick :: PluginIO Nickname
getNick = gets s_nick

sendMsg :: Message -> PluginIO ()
sendMsg msg = gets s_sendMsg >>= \f -> lift $ f msg

sendMsg' :: Message -> Message -> PluginIO ()
sendMsg' msg logMsg = gets s_sendMsg' >>= \f -> lift $ f msg logMsg


type PluginIO = StateT PluginState IO

type PluginFunc = Message -> PluginIO ()

data Plugin = Plugin {
      pluginFunc :: PluginFunc,
      requireTLS :: Bool
    }

simplePlugin :: FromJSON a => (a -> PluginFunc) -> Value -> IO Plugin
simplePlugin f v =
    either error (\x -> return $ Plugin (f x) False) (parseEither parseJSON v)


type Param = BS.ByteString
type Command = BS.ByteString
data Message = Message (Maybe Prefix) Command [Param]
  deriving Show
