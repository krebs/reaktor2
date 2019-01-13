{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.Register (plugin) where

import           Control.Monad (when)
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           GHC.Generics
import           Reaktor.Types
import           Reaktor.Utils (nextNick,randomNick)


data RegisterConfig = RegisterConfig {
      channels :: [BS.ByteString]
    }
  deriving (FromJSON,Generic)


plugin :: Value -> IO Plugin
plugin = simplePlugin run


run :: RegisterConfig -> PluginFunc
run cfg msg = do
    nick_ <- getNick
    case msg of

      Message _ "<start>" _ -> do
        sendMsg (Message Nothing "NICK" [nick_])
        sendMsg (Message Nothing "USER" [nick_, "*", "0", nick_])

      Message (Just _self) "NICK" (newnick:[]) -> do
        when (newnick == nick_) $ do
          -- TODO JOIN only if not already joined
          --      i.e. not during subsequent nick changes
          sendMsg (Message Nothing "JOIN" [ BS.intercalate "," (channels cfg) ])

      -- RFC1459 ERR_NICKNAMEINUSE
      Message (Just _servername) "433" (_msgtarget:nickinuse:_reason:[]) -> do
        if nickinuse == nick_ then do
          let nick' = nextNick nickinuse
          sendMsg (Message Nothing "NICK" [nick'])
          -- TODO change state on "NICK"
          setNick nick'

        -- TODO is this just for NickServ? (also check that module if it has
        -- stuff only for "Register")
        else do
          nick' <- lift randomNick
          sendMsg (Message Nothing "NICK" [nick'])
          -- TODO set nick on "NICK" message
          setNick nick'

      -- RFC2812 ERR_UNAVAILRESOURCE
      --Message (Just _servername) "437" (_msgtarget:nickunavail:_reason:[]) -> do

      -- RFC2812 RPL_WELCOME
      Message _ "001" [_nick,_s] -> do
        --logStrLn $ SGR [32,1] (Plain s)
        sendMsg (Message Nothing "JOIN" [ BS.intercalate "," (channels cfg) ])


      _ -> return ()
