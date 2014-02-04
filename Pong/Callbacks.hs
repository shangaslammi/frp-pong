module Pong.Callbacks
    ( initCallbackRefs
    , renderViewport
    , handleKeyboard
    , handleTick
    ) where

import Control.Monad

import Data.IORef
import Data.Time.Clock.POSIX

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import System.Exit

import Pong.Keyboard
import Pong.Game
import Pong.Rect

import Control.Coroutine

type KeyboardRef = IORef Keyboard
type GameRef     = IORef (Rects, GameLogic)
type TimeRef     = IORef POSIXTime

type CallbackRefs = (TimeRef, KeyboardRef, GameRef)

secPerTick :: Fractional a => a
secPerTick = 0.05

-- | Initialize a new group of callback references
initCallbackRefs :: IO CallbackRefs
initCallbackRefs = do
    next  <- getPOSIXTime >>= newIORef
    keyb  <- newIORef initKeyboard
    cont  <- newIORef ([], game)
    return (next, keyb, cont)

-- | Render the view and swap display buffers
renderViewport :: CallbackRefs -> IO ()
renderViewport (_, _, gr) = do
    (r, _)   <- readIORef gr

    clear [ColorBuffer]

    renderRects r
    -- let interpolation = realToFrac $ accum' / secPerTick
    -- renderInterpolated interpolation s'

    swapBuffers

-- | Run the game logic, set display to rerender
handleTick :: CallbackRefs -> TimerCallback
handleTick refs@(next, kb, gr) = do
  nextT       <- readIORef next
  keys        <- readIORef kb
  (_, c)      <- readIORef gr

  let (r', c') = runC c keys
  newGr <- case c' of
    Just c' -> return (r', c')
    Nothing -> exitWith ExitSuccess

  writeIORef gr newGr
  postRedisplay Nothing

  current     <- getPOSIXTime
  let nextTime = max 0 (secPerTick - (current - nextT))
  writeIORef next (current + nextTime)

  addTimerCallback (round (nextTime * 1000)) (handleTick refs)

-- | Update the Keyboard state according to the event
handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (_, kb, _) k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
