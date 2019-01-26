{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.System.Internal where

import Prelude.Extended
import Data.Aeson
import Reaktor ()



-- TODO this needs better names :)
data CaptureOr a = Capture Int | CaptureOr a
  deriving Show -- TODO killme

instance FromJSON a => FromJSON (CaptureOr a) where
  parseJSON o@(Number _) = Capture <$> parseJSON o -- TODO don't parse twice
  parseJSON o = CaptureOr <$> parseJSON o

-- TODO query means via direct privmsg and <nick>:
data Activate = Always | Match | Query
  deriving Show

instance FromJSON Activate where
  parseJSON = \case
      String "always" -> pure Always
      String "match" -> pure Match
      String "query" -> pure Query
      _ -> undefined

data Config = Config
    { cWorkDir :: Maybe FilePath
    , cHooks :: HashMap Text [Hook]
    }
  deriving Show

instance Default Config where
  def = Config Nothing mempty

instance FromJSON Config where
  parseJSON = \case
      Object v ->
        Config
          <$> v .:? "workdir"
          <*> v .:? "hooks" .!= mempty
      _ -> undefined

data Hook = Hook
    { hActivate :: Activate
    , hPattern :: Maybe ByteString
    , hCommand :: CaptureOr Command
    , hArguments :: [CaptureOr Text]
    , hWorkDir :: Maybe FilePath
    , hCommands :: HashMap Text Command
    , hTimeout :: Maybe Int
    }
  deriving Show

instance FromJSON Hook where
  parseJSON = \case
      Object v ->
        Hook
          <$> v .:? "activate" .!= Query
          <*> v .:? "pattern"
          <*> v .: "command"
          <*> v .:? "arguments" .!= []
          <*> v .:? "workdir"
          <*> v .:? "commands" .!= mempty
          <*> (fmap (*1000000) <$> v .:? "timeoutSec" .!= Just 10)
      _ -> undefined


data Command = Command
    { commandPath :: FilePath
    , commandWorkDir :: Maybe FilePath
    , commandEnv :: Maybe (HashMap String String)
    }
  deriving Show

instance FromJSON Command where
  parseJSON = \case
      Object v ->
        Command
          <$> v .: "filename"
          <*> v .:? "workdir"
          <*> v .:? "env"
      _ -> undefined

