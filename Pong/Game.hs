{-# LANGUAGE Arrows #-}
module Pong.Game (GameLogic, game) where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Pong.Keyboard
import Pong.Controls
import Pong.Rect

import Control.Category (id)
import Prelude hiding (id)

type GameLogic = Coroutine Keyboard Rects

type PlayerPos = Pos
type BallPos   = Pos
type Velocity  = (X,Y)

batSpeed = 5
batSize  = (10,40)

startPos = 200

ballInitPos = (400,200)
ballSize    = (8,8)
ballInitDir = (-1, -1)
ballSpeed   = 6

topWall    = 10
bottomWall = 590

data BallBounce = HBounce | VBounce
type BallReset  = ()

vecMul :: Int -> (Int, Int) -> (Int, Int)
vecMul c (x,y) = (x*c,y*c)

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (a,b) (c,d) = (a+c,b+d)

game :: GameLogic
game = playerPos >>> mkRect batSize &&& (ballPos >>> mkRect ballSize) >>> joinRects

joinRects :: Coroutine (Rect, Rect) Rects
joinRects = arr (\(a,b) -> [a,b])

playerSpeed :: Coroutine Keyboard Int
playerSpeed = arr keyboardDir where
    keyboardDir kb
        | isKeyDown kb up   = -batSpeed
        | isKeyDown kb down = batSpeed
        | otherwise         = 0

playerPos :: Coroutine Keyboard PlayerPos
playerPos = playerSpeed >>> integrate startPos >>> arr (\y -> (10, y))

{-
playerPos :: Coroutine Keyboard PlayerPos
playerPos = proc kb -> do
    dir <- keyboardDir -< kb
    let velocity = dir * batSpeed
    y <- integrate startPos -< velocity
    returnA -< (10, y)
-}

{-
ballPos :: Coroutine PlayerPos BallPos
ballPos = loop $ watch collision &&& arr snd
    >>> mapE (const HBounce) *** wallBounce
    >>> mergeE
    >>> scanE bounce ballInitDir
    >>> arr (vecMul ballSpeed)
    >>> scan vecAdd ballInitPos
    >>> withPrevious ballInitPos
-}

{-
ballPos :: Coroutine PlayerPos BallPos
ballPos = proc plPos -> do
    rec pos   <- restartWhen ballPos' -< (plPos, reset)
        reset <- watch outOfBounds -< pos
    returnA -< pos
    where outOfBounds (x,_) = x < 0 || x > 800
-}

ballPos :: Coroutine PlayerPos BallPos
ballPos = loop $ restartWhen ballPos' >>> id &&& watch outOfBounds
    where outOfBounds (x,_) = x < 0 || x > 800

ballPos' :: Coroutine PlayerPos BallPos
ballPos' = proc plPos -> do
    rec prev  <- delay ballInitPos -< pos
        batB  <- constE HBounce <<< watch collision -< (plPos, prev)
        wallB <- wallBounce -< prev
        dir   <- scanE bounce ballInitDir <<< mergeE -< (batB, wallB)
        let velocity = ballSpeed `vecMul` dir
        pos   <- scan vecAdd ballInitPos -< velocity
    returnA -< pos

collision :: (PlayerPos, BallPos) -> Bool
collision ((px,py),(bx,by)) = abs (px-bx) < w' && abs (py-by) < h' where
    w' = (bw + pw) `div` 2
    h' = (bh + ph) `div` 2
    (bw,bh) = ballSize
    (pw,ph) = batSize

bounce :: Velocity -> BallBounce -> Velocity
bounce (dx,dy) b = case b of
    HBounce -> (-dx,dy)
    VBounce -> (dx,-dy)

wallBounce :: Coroutine BallPos (Event BallBounce)
wallBounce = watch (\(_,y) -> y < topWall || y > bottomWall) >>> constE VBounce

mkRect :: Size -> Coroutine Pos Rect
mkRect (w,h) = arr $ \(x,y) -> ((x-w',y-h'),(w,h)) where
    w' = w `div` 2
    h' = h `div` 2
