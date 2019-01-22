{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Plugins.System.Internal where

import Prelude.Extended
import Data.Aeson
import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.Map as M
import Reaktor ()


-- TODO this needs better names :)
data CaptureOr a = Capture Integer | CaptureOr a
  deriving Show -- TODO killme

instance FromJSON a => FromJSON (CaptureOr a) where
  parseJSON o@(Number _) = Capture <$> parseJSON o -- TODO don't parse twice
  parseJSON o = CaptureOr <$> parseJSON o

-- TODO query means via direct privmsg and <nick>:
data Activate = Always | Match | Query

instance FromJSON Activate where
  parseJSON (String "always") = pure Always
  parseJSON (String "match") = pure Match
  parseJSON (String "query") = pure Query
  parseJSON _ = undefined

data Config = Config {
  cDefaultWorkDir :: Maybe FilePath,
  -- TODO IrcCommand as key for map
  cHooks :: M.Map BS.ByteString [SystemParams]
}

instance Default Config where
  def = Config Nothing mempty

instance FromJSON Config where
  parseJSON (Object v) =
    Config
      <$> v .:? "workdir"
      <*> v .:? "hooks" .!= M.empty
  parseJSON _ = pure undefined

data SystemParams = SystemParams {
  activate :: Activate,
  pattern :: Maybe BS.ByteString, -- TODO RE
  command :: CaptureOr SystemCommand,
  arguments :: [CaptureOr BS.ByteString],
  workDir :: Maybe FilePath,
  commands :: M.Map BS.ByteString SystemCommand
}

instance FromJSON SystemParams where
  parseJSON (Object v) =
      SystemParams
        <$> v .:? "activate" .!= Query
        <*> v .:? "pattern"
        <*> v .: "command"
        <*> v .:? "arguments" .!= []
        <*> v .:? "workdir"
        <*> v .:? "commands" .!= M.empty
  parseJSON _ = pure undefined


data SystemCommand = SystemCommand {
    commandPath :: FilePath,
    commandWorkDir :: Maybe FilePath,
    commandEnv :: Maybe (M.Map String String)
  }
  deriving Show -- TODO killme

instance FromJSON SystemCommand where
  parseJSON (Object v) =
      SystemCommand
        <$> v .: "filename"
        <*> v .:? "workdir"
        <*> v .:? "env"
  parseJSON _ = pure undefined

