module Control.Concurrent.Extended
    ( module Exports
    , newChan
    , newRef
    , newRelay
    , newSemaphore
    ) where

import Control.Arrow
import Control.Concurrent as Exports hiding (newChan,readChan,writeChan)
import qualified Control.Concurrent.Chan.Unagi as U
import Data.IORef

newChan :: IO (a -> IO (), IO a)
newChan = (U.writeChan *** U.readChan) <$> U.newChan

newRef :: a -> IO (a -> IO (), IO a)
newRef v0 = (atomicWriteIORef &&& readIORef) <$> newIORef v0

newRelay :: IO (a -> IO (), IO a)
newRelay = (putMVar &&& takeMVar) <$> newEmptyMVar

newSemaphore :: IO (IO (), IO ())
newSemaphore = first ($()) <$> newRelay
