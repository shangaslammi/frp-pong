module Pong.Controls
    ( up
    , down
    ) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

up   = SpecialKey KeyUp
down = SpecialKey KeyDown
