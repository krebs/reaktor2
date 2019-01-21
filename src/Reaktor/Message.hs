{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Message where

import qualified Data.ByteString.Char8.Extended as BS
import           Reaktor.Types


privmsg :: BS.ByteString -> [BS.ByteString] -> Message
privmsg msgtarget xs =
    Message Nothing "PRIVMSG" (msgtarget:BS.intercalate " " xs:[])

notice :: BS.ByteString -> [BS.ByteString] -> Message
notice msgtarget xs =
    Message Nothing "NOTICE" (msgtarget:BS.intercalate " " xs:[])
