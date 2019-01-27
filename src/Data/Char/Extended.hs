module Data.Char.Extended
    ( module Data.Char
    , isAsciiLetter
    , isNick
    ) where

import Data.Char

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiUpper c || isAsciiLower c

-- RFC1459
isNick :: Char -> Bool
isNick c =
    0x41 <= i && i <= 0x7D || -- letters / specials
    0x30 <= i && i <= 0x39 || -- digits
    0x2D == i                 -- minus
  where
    i = ord c
