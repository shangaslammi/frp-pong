
module Pong.Rect where

import Graphics.Rendering.OpenGL hiding (Rect, Size)

type Rects = [Rect]
type Rect = (Pos,Size)
type Pos = (X,Y)
type Size = (W,H)
type X = Int
type Y = Int
type W = Int
type H = Int


renderRects :: Rects -> IO ()
renderRects rects = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    let vtx :: Int -> Int -> Vertex2 Float
        vtx x y = Vertex2 (fromIntegral x) (fromIntegral y)
        rectVertices ((x,y),(w,h)) = do
            vertex $ vtx x y
            vertex $ vtx (x+w) y
            vertex $ vtx (x+w) (y+h)
            vertex $ vtx x (y+h)

    renderPrimitive Quads $ mapM_ rectVertices rects
