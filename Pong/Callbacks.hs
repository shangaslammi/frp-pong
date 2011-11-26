module Pong.Callbacks
    ( initCallbackRefs
    , renderViewport
    , handleKeyboard
    ) where

import Data.IORef
import Data.Time.Clock.POSIX

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Pong.Keyboard
import Pong.Game
import Pong.Rect

import Control.Coroutine

type KeyboardRef = IORef Keyboard
type TimeRef     = IORef POSIXTime
type AccumRef    = TimeRef
type PrevTimeRef = TimeRef
type GameRef     = IORef (Rects, GameLogic)

type CallbackRefs = (AccumRef, PrevTimeRef, KeyboardRef, GameRef)

secPerTick :: Fractional a => a
secPerTick = 0.05

maxFrameTime :: Fractional a => a
maxFrameTime = 0.05

-- | Initialize a new group of callback references
initCallbackRefs :: IO CallbackRefs
initCallbackRefs = do
    accum <- newIORef secPerTick
    prev  <- getPOSIXTime >>= newIORef
    keyb  <- newIORef initKeyboard
    cont  <- newIORef ([],game)
    return (accum, prev, keyb, cont)

-- | Run the game logic, render the view and swap display buffers
renderViewport :: CallbackRefs -> IO ()
renderViewport (ar, pr, kb, gr) = do
    current <- getPOSIXTime
    accum   <- readIORef ar
    prev    <- readIORef pr
    keys    <- readIORef kb
    (r,c)   <- readIORef gr

    let delta  = accum + min maxFrameTime (current - prev)

    (r', accum') <- if delta >= secPerTick
        then do
            let (r', c') = runC c keys
            writeIORef gr (r',c')
            return (r', delta - secPerTick)
        else return (r, delta)

    writeIORef ar accum'
    writeIORef pr current

    clear [ColorBuffer]

    renderRects r'
    -- let interpolation = realToFrac $ accum' / secPerTick
    -- renderInterpolated interpolation s'

    swapBuffers
    postRedisplay Nothing

-- | Update the Keyboard state according to the event
handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (_, _, kb, _) k ks _ _ = modifyIORef kb (handleKeyEvent k ks)
