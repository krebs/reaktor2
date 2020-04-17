module System.Posix.Files.Extended
    ( module Exports
    , removeIfExists
    ) where

import qualified System.Posix.Files as Exports

import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (removeLink)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeLink fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

