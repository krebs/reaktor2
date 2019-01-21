{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.NickServ (plugin) where

import           Control.Monad (when)
import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8.Extended as BS
import           GHC.Generics
import           Reaktor.Internal
import           Reaktor.Message
import           Reaktor.Utils (randomNick)


data NickServConfig = NickServConfig {
      passFile :: FilePath,
      prefix :: BS.ByteString,
      channels :: [BS.ByteString]
    }
  deriving (FromJSON,Generic)


plugin :: Value -> IO Plugin
plugin v =
    case parseEither parseJSON v of
      Right cfg -> do
        pass <- do
          [pass] <- lines <$> readFile (passFile cfg)
          return (BS.pack pass)

        return $ Plugin (run pass cfg) True
      Left err ->
        error err


run :: BS.ByteString -> NickServConfig -> PluginFunc
run pass cfg msg = do
    nick_ <- getNick
    case msg of

      Message _ "<start>" _ -> do
        nick0 <- lift randomNick
        sendMsg (Message Nothing "NICK" [nick0])
        sendMsg (Message Nothing "USER" [nick_, "*", "0", nick_])

      -- TODO structured prefix, and check just for "NickServ"
      Message (Just _prefix@"NickServ!NickServ@services.")
                       "NOTICE"
                       (_msgtarget:text:[]) -> do
       if
         | text == "You are now identified for \STX" <> nick_ <> "\STX." -> do
           sendMsg (Message Nothing "NICK" [nick_])
         | text == "\STX" <> nick_ <> "\STX has been released." -> do
           sendMsg (Message Nothing "NICK" [nick_])
         | text == "Invalid password for \STX" <> nick_ <> "\STX." -> do
           error (BS.unpack text)
         | text == "\STX" <> nick_ <> "\STX is not a registered nickname." -> do
           error (BS.unpack text)
         | otherwise ->
           return ()


      Message (Just _self) "NICK" (newnick:[]) -> do
        when (newnick == nick_) $ do
          -- TODO JOIN only if not already joined
          --      i.e. not during subsequent nick changes
          sendMsg (Message Nothing "JOIN" [ BS.intercalate "," (channels cfg) ])


      -- RFC1459 ERR_NICKNAMEINUSE
      Message (Just _servername) "433" (_msgtarget:nickinuse:_reason:[]) -> do
        if nickinuse == nick_
          then do
            sendMsg (privmsg "NickServ" ["RELEASE", nickinuse])
          else do
            nick0 <- lift randomNick
            sendMsg (Message Nothing "NICK" [nick0])

      --RFC2812 ERR_UNAVAILRESOURCE
      Message (Just _servername) "437" (_msgtarget:nickunavail:_reason:[]) -> do
        when (nickunavail == nick_) $ do
          sendMsg (privmsg "NickServ" ["RELEASE", nickunavail])

      --RFC2812 RPL_WELCOME
      Message _ "001" [_nick,_s] -> do
        sendMsg' (privmsg "NickServ" ["IDENTIFY", nick_, pass])
                 (privmsg "NickServ" ["IDENTIFY", nick_, "<password>"])


      _ -> return ()
