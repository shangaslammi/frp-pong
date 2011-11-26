
module Control.CoroutineM where

import Data.Functor
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad

import Prelude hiding (id, (.))

newtype CoroutineM m i o = CoroutineM { runCM :: i -> m (o, CoroutineM m i o) }

instance Monad m => Functor (CoroutineM m i) where
    fmap f co = CoroutineM $ \i -> do
        (o, co') <- runCM co i
        return (f o, fmap f co')

instance Monad m => Applicative (CoroutineM m i) where
    pure x = CoroutineM $ const $ return (x, pure x)

    cof <*> cox = CoroutineM $ \i -> do
        (f, cof') <- runCM cof i
        (x, cox') <- runCM cox i
        return (f x, cof' <*> cox')

instance Monad m => Category (CoroutineM m) where
    id = CoroutineM $ \i -> return (i, id)

    cof . cog = CoroutineM $ \i -> do
        (x, cog') <- runCM cog i
        (y, cof') <- runCM cof x
        return (y, cof' . cog')

instance Monad m => Arrow (CoroutineM m) where
    arr f = CoroutineM $ \i -> return (f i, arr f)

    first co = CoroutineM $ \(a,b) -> do
        (c, co') <- runCM co a
        return ((c,b), first co')
