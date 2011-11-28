{-# LANGUAGE BangPatterns #-}

module Control.Coroutine.FRP where

import qualified Control.Category as C
import Control.Arrow
import Data.List (foldl')

import Control.Coroutine
import qualified Data.IntMap as IntMap

type Event a = [a]

edge :: Eq a => Coroutine a (Event a)
edge = Coroutine $ \i -> ([i], step i) where
    step old = Coroutine $ \i ->
        if old == i
            then ([],  step i)
            else ([i], step i)

watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], watch f)
        else ([], watch f)

withPrevious :: a -> Coroutine a (a,a)
withPrevious first = Coroutine $ \i -> ((i, first), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

withPrevious' :: Coroutine a (a,a)
withPrevious' = Coroutine $ \i -> ((i,i), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

integral :: Num a => a -> Coroutine a a
integral = scan (+)

derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipC (-)

scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a e = let a' = foldl' f a e in (a', scanE f a')

mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

concatMapE :: (e -> Event e') -> Coroutine (Event e) (Event e')
concatMapE = arr . concatMap

filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

mergeE :: Coroutine (Event e, Event e) (Event e)
mergeE = zipC (++)

stepE :: a -> Coroutine (Event a) a
stepE a = Coroutine $ \ev ->
    let a' = last (a:ev)
    in (a', stepE a')

restartWhen :: Coroutine a b -> Coroutine (Event e, a) b
restartWhen co = Coroutine $ step co where
    step c (ev, i)
        | null ev   = let (o, c') = runC c i in (o, Coroutine $ step c')
        | otherwise = let (o, c') = runC co i in (o, restartWhen co)

delayE :: Int -> Coroutine (Event e) (Event e)
delayE delay = arr (const delay) &&& C.id >>> delayEn

delayEn :: Coroutine (Int, Event e) (Event e)
delayEn = Coroutine $ step 0 IntMap.empty where
    step !cur !buffer (delay, ev) = (ev', Coroutine $ step (cur+1) buffer') where
        ev' = IntMap.findWithDefault [] cur buffer'
        buffer'
            | null ev   = buffer
            | otherwise = IntMap.insertWith (++) (cur+delay) ev buffer
