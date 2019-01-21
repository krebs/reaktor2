{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.Mention (plugin) where

import           Control.Monad (when)
import           Data.Aeson
import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.Char
import           Reaktor.Message
import           Reaktor.Types


plugin :: Value -> IO Plugin
plugin _ = return (Plugin run False)


run :: PluginFunc
run = \case
    Message _ "PRIVMSG" (msgtarget:text:[]) -> do
        nick <- getNick
        when (isMention nick text) $ do
          sendMsg (privmsg msgtarget ["I'm famous!"])
    _ -> return ()
  where
    isMention nick text =
      not (BS.isPrefixOf (nick <> ":") text) &&
      any (==nick) (BS.splitWith (not . Data.Char.isAlphaNum) text)
