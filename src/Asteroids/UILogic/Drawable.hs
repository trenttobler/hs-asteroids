module Asteroids.UILogic.Drawable (
  Drawable(..),
  Coord,
  Poly2,
  pt2ToPoly,
  drawPoly, fillPoly,
  drawColor,
  innerDrawing, adjustOrigin
) where

import           Graphics.Rendering.OpenGL
import           Pt2

class Drawable x where
    draw :: x -> IO ()

newtype Poly2 = Poly2 (IO ())

pt2ToPoly::[Pt2 Coord] -> Poly2
pt2ToPoly ps = Poly2 $ mapM_ mapGLPt2 ps

drawPoly :: Poly2 -> IO ()
drawPoly (Poly2 p) = renderPrimitive LineLoop p

fillPoly :: Poly2 -> IO ()
fillPoly (Poly2 p) = renderPrimitive Polygon p


drawColor::GLfloat->GLfloat->GLfloat->IO ()
drawColor x y z = color $ Color3 x y z

type Coord = GLfloat

coordToGL:: Coord -> GLfloat
coordToGL = id

ptVertex2 :: Pt2 Coord -> Vertex2 GLfloat
ptVertex2 (Pt2 (x,y)) = Vertex2 (coordToGL x) (coordToGL y)

ptVector3 :: Pt2 Coord -> Vector3 GLfloat
ptVector3 (Pt2 (x,y)) = Vector3 (coordToGL x) (coordToGL y) 0

mapGLPt2::Pt2 Coord -> IO ()
mapGLPt2 = vertex . ptVertex2

adjustOrigin :: Pt2 Coord -> Coord -> IO ()
adjustOrigin p r = do
        translate $ ptVector3 p
        rotate (coordToGL r) $ Vector3 0 0 (1::GLfloat)

innerDrawing :: IO () -> IO ()
innerDrawing f = preservingMatrix f