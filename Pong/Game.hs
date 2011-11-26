
module Pong.Game where

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
type Velocity = (X,Y)

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
game = playerPos >>> gameLogic >>> mkRects where

    gameLogic :: Coroutine PlayerPos (PlayerPos, BallPos)
    gameLogic = id &&& ballPos

    mkRects :: Coroutine (PlayerPos, BallPos) Rects
    mkRects = batRect *** ballRect
        >>> arr (\(a,b) -> [a,b])

    keyboardDir :: Coroutine Keyboard Int
    keyboardDir = arr f where
        f kb
            | isKeyDown kb up   = -1
            | isKeyDown kb down = 1
            | otherwise         = 0

    playerPos :: Coroutine Keyboard PlayerPos
    playerPos = keyboardDir
        >>> arr (*batSpeed)
        >>> integral startPos
        >>> arr (const 10) &&& id

    batRect :: Coroutine Pos Rect
    batRect = arr $ \(x,y) -> ((x-w',y-h'),(w,h)) where
        (w,h) = batSize
        w' = w `div` 2
        h' = h `div` 2

    -- ballPos :: Coroutine (Event BallBounce, Event BallReset) Pos
    -- ballPos = scanE ballInitPos

    ballPos :: Coroutine PlayerPos BallPos
    ballPos = loop $ watch collision &&& arr snd
        >>> mapE (const HBounce) *** wallBounce
        >>> mergeE
        >>> scanE bounce ballInitDir
        >>> arr (vecMul ballSpeed)
        >>> scan vecAdd ballInitPos
        >>> withPrevious ballInitPos

    collision :: (PlayerPos, BallPos) -> Bool
    collision ((px,py),(bx,by)) = abs (px-bx) < w' && abs (py-by) < h' where
        w' = (bw + pw) `div` 2
        h' = (bh + ph) `div` 2
        (bw,bh) = ballSize
        (pw,ph) = batSize

    bounce :: (Int,Int) -> BallBounce -> (Int,Int)
    bounce (dx,dy) b = case b of
        HBounce -> (-dx,dy)
        VBounce -> (dx,-dy)

    wallBounce :: Coroutine Pos (Event BallBounce)
    wallBounce = watch (\(_,y) -> y < topWall || y > bottomWall)
        >>> mapE (const VBounce)

    ballRect :: Coroutine Pos Rect
    ballRect = arr $ \(x,y) -> ((x-w',y-h'),(w,h)) where
        (w,h) = ballSize
        w' = w `div` 2
        h' = h `div` 2