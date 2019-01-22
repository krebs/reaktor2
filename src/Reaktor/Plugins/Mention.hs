{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.Mention (new) where

import Prelude.Extended
import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.Char
import Reaktor


new :: Actions -> IO (Message -> IO ())
new Actions{..} = do
    pure $ \case
      Message _ "PRIVMSG" (msgtarget:text:[]) -> do
          nick <- aGetNick
          when (isMention nick text) $ do
            aSend (privmsg msgtarget ["I'm famous!"])
      _ -> return ()
  where
    isMention nick text =
      not (BS.isPrefixOf (nick <> ":") text) &&
      any (==nick) (BS.splitWith (not . Data.Char.isAlphaNum) text)
