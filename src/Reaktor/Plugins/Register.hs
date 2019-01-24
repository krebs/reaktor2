{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.Register where

import Blessings
import Prelude.Extended
import Data.Aeson
import Data.ByteString.Char8.Extended (ByteString)
import qualified Data.ByteString.Char8.Extended as BS
import qualified Reaktor.Nick as Nick
import Reaktor
import System.Environment (lookupEnv)

data ConfigNickServ = ConfigNickServ
    { cnsPassFile :: FilePath
    , cnsPrefix :: ByteString
    }
instance FromJSON ConfigNickServ where
  parseJSON = \case
    Object v ->
      ConfigNickServ
        <$> v .: "passFile"
        <*> v .:? "prefix" .!= "NickServ!NickServ@services."
    _ -> undefined

data Config = Config
    { cNick :: Maybe ByteString
    , cUser :: Maybe ByteString
    , cReal :: ByteString
    , cChannels :: [ByteString]
    , cNickServ :: Maybe ConfigNickServ
    }
instance Default Config where
  def = Config def def "reaktor2" def def
instance FromJSON Config where
  parseJSON = \case
    Object v -> do
      cNick <- v .:? "nick" .!= Nothing
      cUser <- v .:? "user"
      cReal <- v .:? "real" .!= cReal def
      cChannels <- v .:? "channels" .!= []
      cNickServ <- v .:? "NickServ" .!= cNickServ def
      pure Config{..}
    _ -> undefined

new :: Config -> Actions -> IO (Message -> IO ())
new Config{..} Actions{..} = do
    let
        isNickServEnabled = aIsSecure && isJust cNickServ
        Just ConfigNickServ{..} = cNickServ

        release nick pass = do
          aSend (privmsg "NickServ" ["RELEASE", nick, pass])

        channelsArg = BS.intercalate "," cChannels
        -- TODO make this similar to privmsg (i.e. don't aSend)
        join = do
            -- TODO JOIN only if not already joined
            --      i.e. not during subsequent nick changes
            unless (BS.null channelsArg) $
              aSend (Message Nothing "JOIN" [channelsArg])

        start = do
          nick <- maybe aGetNick pure cNick
          user <-
            maybe (maybe nick BS.pack <$> lookupEnv "LOGNAME") pure cUser
          aSetNick nick
          aSend (Message Nothing "NICK" [nick])
          aSend (Message Nothing "USER" [user, "*", "0", cReal])
        onNick newnick = do
          nick <- aGetNick
          when (newnick == nick) join
        useRandomNick = do
          nick <- Nick.getRandom
          aSetNick nick
          aSend (Message Nothing "NICK" [nick])
        useNextNick = do
          nick0 <- aGetNick
          let nick = Nick.getNext nick0
          aSetNick nick
          aSend (Message Nothing "NICK" [nick])
        useNextNickTemporarily = do
          nick <- aGetNick
          let tmpNick = Nick.getNext nick
          -- do not aSetNick tmpNick 
          aSend (Message Nothing "NICK" [tmpNick])

    if not isNickServEnabled then do
      when (isJust cNickServ) $ do
        aLog $ SGR [38,5,202] "! disabling NickServ due to insecure connection"
      pure $ \case
        Start -> start
        Message (Just _self) "NICK" (newnick:[]) -> onNick newnick
        Message _ "001" _ -> join
        Message _ "432" _ -> useRandomNick
        Message _ "433" _ -> useNextNick
        Message _ "437" (_msgtarget:res:_reason:[]) -> do
          nick <- aGetNick
          when (res == nick) useNextNick
        _ -> pure ()

    else do
      -- TODO do not fail, but disable NicServ
      [pass] <- BS.lines <$> BS.readFile cnsPassFile
      pure $ \case
        Start -> start
        Message (Just _self) "NICK" (newnick:[]) -> onNick newnick

        -- RFC2812 RPL_WELCOME
        Message _ "001" [msgtarget,_text] -> do
          nick <- aGetNick
          aSend (privmsg "NickServ" ["IDENTIFY", nick, pass])
          when (msgtarget /= nick) (release nick pass)

        -- TODO structured prefix, and check just for "NickServ"?
        Message (Just prefix) "NOTICE" (msgtarget:text:[]) ->
          when (prefix == cnsPrefix) $ do
            nick <- aGetNick
            let stx = ("\STX"<>) . (<>"\STX")
            if
              | text == "You are now identified for " <> stx nick <> "." -> do
                -- otherwise join at NICK
                when (msgtarget == nick) join

              | text == stx nick <> " has been released." -> do
                aSend (Message Nothing "NICK" [nick])

              | text == "Invalid password for " <> stx nick <> "." -> do
                -- TODO warning
                when (msgtarget == nick) join

              | text == stx nick <> " is not a registered nickname." -> do
                -- TODO warning
                when (msgtarget == nick) join

              | otherwise ->
                pure ()

        -- RFC1459 ERR_ERRONEUSNICKNAME
        Message (Just _servername) "432" (_msgtarget:_nick:_reason:[]) ->
          useRandomNick

        -- RFC1459 ERR_NICKNAMEINUSE
        Message (Just _servername) "433" (_msgtarget:_nick:_reason:[]) ->
          -- TODO what if nick0 /= nick? OR assert/prove nick0 == nick?
          useNextNickTemporarily

        --RFC2812 ERR_UNAVAILRESOURCE
        Message (Just _servername) "437" (msgtarget:res:_reason:[]) -> do
          nick <- aGetNick
          when (res == nick) $
            case msgtarget of
              "*" -> useNextNickTemporarily
              _ -> release nick pass

        _ -> pure ()
