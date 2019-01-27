{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.Mention (new) where

import qualified Data.Char
import qualified Data.Text as T
import Prelude.Extended
import Reaktor


new :: Actions -> IO (Message -> IO ())
new Actions{..} = do
    pure $ \case
      Message _ PRIVMSG (msgtarget:text:[]) -> do
          nick <- aGetNick
          when (isMention nick text) $ do
            aSend (privmsg msgtarget ["I'm famous!"])
      _ -> return ()
  where
    isMention nick text =
      not (T.isPrefixOf (nick <> ":") text) &&
      any (==nick) (T.split (not . Data.Char.isAlphaNum) text)
