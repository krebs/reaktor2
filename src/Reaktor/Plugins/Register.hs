{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.Register where

import Blessings
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude.Extended
import Reaktor
import qualified Reaktor.Nick as Nick
import System.Environment (lookupEnv)

data ConfigNickServ = ConfigNickServ
    { cnsPassFile :: FilePath
    , cnsPrefix :: Text
    }
instance FromJSON ConfigNickServ where
  parseJSON = \case
    Object v ->
      ConfigNickServ
        <$> v .: "passFile"
        <*> v .:? "prefix" .!= "NickServ!NickServ@services."
    invalid -> typeMismatch "ConfigNickServ" invalid

data Config = Config
    { cNick :: Maybe Text
    , cUser :: Maybe Text
    , cReal :: Text
    , cChannels :: [Text]
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
    invalid -> typeMismatch "Config" invalid

new :: Config -> Actions -> IO (Message -> IO ())
new Config{..} Actions{..} = do
    let
        isNickServEnabled = aIsSecure && isJust cNickServ
        Just ConfigNickServ{..} = cNickServ

        regain nick pass = do
          aSend (privmsg "NickServ" ["REGAIN", nick, pass])

        channelsArg = T.intercalate "," cChannels
        -- TODO make this similar to privmsg (i.e. don't aSend)
        join = do
            -- TODO JOIN only if not already joined
            --      i.e. not during subsequent nick changes
            unless (T.null channelsArg) $
              aSend (Message Nothing JOIN [channelsArg])

        start = do
          nick <- maybe aGetNick pure cNick
          user <-
            maybe (maybe nick T.pack <$> lookupEnv "LOGNAME") pure cUser
          aSetNick nick
          aSend (Message Nothing NICK [nick])
          aSend (Message Nothing USER [user, "*", "0", cReal])
        onNick newnick = do
          nick <- aGetNick
          when (newnick == nick) join
        useRandomNick = do
          nick <- Nick.getRandom
          aSetNick nick
          aSend (Message Nothing NICK [nick])
        useNextNick = do
          nick0 <- aGetNick
          let nick = Nick.getNext nick0
          aSetNick nick
          aSend (Message Nothing NICK [nick])
        useNextNickTemporarily = do
          nick <- aGetNick
          let tmpNick = Nick.getNext nick
          -- do not aSetNick tmpNick 
          aSend (Message Nothing NICK [tmpNick])

    if not isNickServEnabled then do
      when (isJust cNickServ) $ do
        aLog $ SGR [38,5,202] "! disabling NickServ due to insecure connection"
      pure $ \case
        Start -> start
        Message (Just _self) NICK (newnick:[]) -> onNick newnick
        Message _ RPL_WELCOME _ -> join
        Message _ ERR_ERRONEUSNICKNAME _ -> useRandomNick
        Message _ ERR_NICKNAMEINUSE _ -> useNextNick
        Message _ ERR_UNAVAILRESOURCE (_msgtarget:res:_reason:[]) -> do
          nick <- aGetNick
          when (res == nick) useNextNick
        _ -> pure ()

    else do
      -- TODO do not fail, but disable NicServ
      [pass] <- T.lines <$> T.readFile cnsPassFile
      pure $ \case
        Start -> start
        Message (Just _self) NICK (newnick:[]) -> onNick newnick

        Message _ RPL_WELCOME [msgtarget,_text] -> do
          nick <- aGetNick
          aSend (privmsg "NickServ" ["IDENTIFY", nick, pass])
          when (msgtarget /= nick) (regain nick pass)

        -- TODO structured prefix, and check just for "NickServ"?
        Message (Just prefix) NOTICE (msgtarget:text:[]) ->
          when (prefix == cnsPrefix) $ do
            nick <- aGetNick
            let stx = ("\STX"<>) . (<>"\STX")
            if
              | text == "You are now identified for " <> stx nick <> "." -> do
                -- otherwise join at NICK
                when (msgtarget == nick) join

              | text == "Invalid password for " <> stx nick <> "." -> do
                -- TODO warning
                when (msgtarget == nick) join

              | text == stx nick <> " is not a registered nickname." -> do
                -- TODO warning
                when (msgtarget == nick) join

              | otherwise ->
                pure ()

        Message _ ERR_ERRONEUSNICKNAME (_msgtarget:_nick:_reason:[]) ->
          useRandomNick

        Message _ ERR_NICKNAMEINUSE (_msgtarget:_nick:_reason:[]) ->
          -- TODO what if nick0 /= nick? OR assert/prove nick0 == nick?
          useNextNickTemporarily

        Message _ ERR_UNAVAILRESOURCE (msgtarget:res:_reason:[]) -> do
          nick <- aGetNick
          when (res == nick) $
            case msgtarget of
              "*" -> useNextNickTemporarily
              _ -> regain nick pass

        _ -> pure ()
