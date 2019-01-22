{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Reaktor.Plugins.Ping where

import Prelude.Extended
import Reaktor

new :: Actions -> IO (Message -> IO ())
new Actions{..} =
    return $ \case
      Message _ cmd args ->
        when (cmd == "PING") $
          aSend (Message Nothing "PONG" args)
      _ -> pure ()
