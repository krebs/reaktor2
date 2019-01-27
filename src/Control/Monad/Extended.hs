module Control.Monad.Extended
    ( module Control.Monad
    , unlessM_
    , untilM_
    ) where

import Control.Monad


unlessM_ :: Monad m => m Bool -> m () -> m ()
unlessM_ p f = p >>= flip unless f

untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ p f = unlessM_ p (f >> untilM_ p f)
