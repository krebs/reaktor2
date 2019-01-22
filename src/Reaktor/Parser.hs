{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Parser where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
--import qualified Data.ByteString.Char8.Extended as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char
import Reaktor.Internal


prefix :: Parser ByteString
prefix = BS.pack <$> many (satisfy Data.Char.isAlphaNum <|>
                           satisfy (flip elem (":.-@/!~[]\\`_^{|}" :: String)))

command :: Parser ByteString
command = BS.pack <$> many1 (satisfy Data.Char.isAlphaNum)

nospcrlfcl :: Parser Char
nospcrlfcl =
  satisfy (flip notElem ("\NUL\CR\LF :" :: String)) <?> "nospcrlfcl"

middle :: Parser ByteString
middle =
    BS.pack <$> ((:) <$> nospcrlfcl <*> many (char ':' <|> nospcrlfcl))
    <?> "middle"

trailing :: Parser ByteString
trailing =
    BS.pack <$> many (char ':' <|> char ' ' <|> nospcrlfcl)
    <?> "trailing"

params :: Parser [ByteString]
params = (do
    a <- many (char ' ' *> middle)
    b <- optional (char ' ' *> char ':' *> trailing)
    return $ a <> (maybe [] (:[]) b))
    <?> "params"

message :: Parser Message
message =
    Message
      <$> optional (char ':' *> prefix <* char ' ')
      <*> command
      <*> params
      <* string "\r\n"
