module Data.Char.Extended
    ( module Data.Char
    , isAsciiLetter
    ) where

import Data.Char

isAsciiLetter :: Char -> Bool
isAsciiLetter c = Data.Char.isAsciiUpper c || Data.Char.isAsciiLower c
