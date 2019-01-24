{-# LANGUAGE OverloadedStrings #-}
module Reaktor.Parser where

import Prelude.Extended
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Char
import qualified Data.Text.Extended as T
import Reaktor.Internal


prefix :: Parser Text
prefix = T.pack <$> many (satisfy Data.Char.isAlphaNum <|>
                           satisfy (flip elem (":.-@/!~[]\\`_^{|}" :: String)))

command :: Parser Text
command = T.pack <$> many1 (satisfy Data.Char.isAlphaNum)

nospcrlfcl :: Parser Char
nospcrlfcl =
  satisfy (flip notElem ("\NUL\CR\LF :" :: String)) <?> "nospcrlfcl"

middle :: Parser Text
middle =
    T.pack <$> ((:) <$> nospcrlfcl <*> many (char ':' <|> nospcrlfcl))
    <?> "middle"

trailing :: Parser Text
trailing =
    T.pack <$> many (char ':' <|> char ' ' <|> nospcrlfcl)
    <?> "trailing"

params :: Parser [Text]
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
