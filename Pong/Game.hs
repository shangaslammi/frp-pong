{-# LANGUAGE Arrows #-}
module Pong.Game (GameLogic, game) where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Data.Monoid

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
ballInitVel = (-6, -6)

topWall    = 10
bottomWall = 590

data BallBounce = HBounce | VBounce
type BallReset  = ()

game :: Coroutine Keyboard Rects
game = proc kb -> do
    plPos <- playerPos -< kb
    blPos <- resettingBallPos -< plPos
    returnA -< [mkRect plPos batSize, mkRect blPos ballSize]

playerSpeed :: Coroutine Keyboard Int
playerSpeed = arr keyboardDir where
    keyboardDir kb
        | isKeyDown kb up   = -batSpeed
        | isKeyDown kb down = batSpeed
        | otherwise         = 0

playerPos :: Coroutine Keyboard PlayerPos
playerPos = playerSpeed >>> integrate startPos >>> arr (\y -> (10, y))

ballPos :: Coroutine PlayerPos BallPos
ballPos = proc plPos -> do
    rec bounces <- (batBounce -< (plPos, pos)) <++> (wallBounce -< pos)
        vel     <- scanE bounce ballInitVel -< bounces
        pos     <- delay ballInitPos <<< scan vecAdd ballInitPos -< vel

    returnA -< pos

resettingBallPos :: Coroutine PlayerPos BallPos
resettingBallPos = proc plPos -> do
    rec pos   <- restartWhen ballPos -< (plPos, reset)
        reset <- watch outOfBounds -< pos
    returnA -< pos
    where outOfBounds (x,_) = x < 0 || x > 800

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

batBounce :: Coroutine (PlayerPos, BallPos) (Event BallBounce)
batBounce = watch collision >>> constE HBounce

vecMul :: Int -> (Int, Int) -> (Int, Int)
vecMul c (x,y) = (x*c,y*c)

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (a,b) (c,d) = (a+c,b+d)

mkRect :: Pos -> Size -> Rect
mkRect (x,y) (w,h) = ((x-w',y-h'),(w,h)) where
    w' = w `div` 2
    h' = h `div` 2
