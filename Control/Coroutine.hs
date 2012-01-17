
module Control.Coroutine where

import Prelude hiding (id, (.))

import Data.Functor
import Control.Applicative
import Control.Arrow
import Control.Category

newtype Coroutine i o = Coroutine { runC :: i -> (o, Maybe (Coroutine i o)) }

instance Functor (Coroutine i) where
    fmap f co = Coroutine $ \i ->
        let (o, co') = runC co i
        in (f o, fmap (fmap f) co')

instance Applicative (Coroutine i) where
    pure x = Coroutine $ const (x, Just $ pure x)

    cof <*> cox = Coroutine $ \i ->
        let (f, cof') = runC cof i
            (x, cox') = runC cox i
        in (f x, (<*>) <$> cof' <*> cox')

instance Category Coroutine where
    id = Coroutine $ \i -> (i, Just id)

    cof . cog = Coroutine $ \i ->
        let (x, cog') = runC cog i
            (y, cof') = runC cof x
        in (y, (.) <$> cof' <*> cog')

instance Arrow Coroutine where
    arr f = Coroutine $ \i -> (f i, Just $ arr f)

    first co = Coroutine $ \(a,b) ->
        let (c, co') = runC co a
        in ((c,b), fmap first co')

instance ArrowLoop Coroutine where
    loop co = Coroutine $ \b ->
        let ((c,d),co') = runC co (b,d)
        in (c, fmap loop co')

instance ArrowChoice Coroutine where
    left co = Coroutine step where
        step ebd = case ebd of
            Left b  -> let (o, co') = runC co b in (Left o, fmap left co')
            Right c -> (Right c, Just $ Coroutine step)

scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i where
    step a b = let a' = f a b in (a', Just $ scan f a')

zipWithC :: (a -> b -> c) -> Coroutine (a,b) c
zipWithC = arr . uncurry

repeatC :: Coroutine a b -> Coroutine a b
repeatC co = Coroutine $ \a ->
    let (b, co') = runC co a
    in (b, co' <|> Just co)

appendC :: Coroutine a b -> Coroutine a b -> Coroutine a b
appendC coa cob = Coroutine $ \a ->
    let (b, co') = runC coa a
    in  (b, co' <|> Just cob)

{-
evalList :: Coroutine i o -> [i] -> [o]
evalList _  []     = []
evalList co (x:xs) = o:evalList co' xs
    where (o, co') = runC co x
-}
