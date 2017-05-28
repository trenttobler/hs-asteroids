module Asteroids.UILogic.Drawable
  ( module PolyPt2
  , Drawable(..)
  , Coord, RgbType
  , Poly2
  , pt2ToPoly
  , drawPoints, drawPoly, fillPoly, drawLine
  , drawColor
  , innerDrawing, adjustOrigin, moveTo
  , PixelColor
  , pixelColor, pixelR, pixelG, pixelB, whitePixel, blackPixel
  ) where

import           Graphics.Rendering.OpenGL
import           PolyPt2

class Drawable x where
    draw :: x -> IO ()

newtype Poly2 = Poly2 (IO ())

pt2ToPoly::[Pt2 Coord] -> Poly2
pt2ToPoly ps = Poly2 $ mapM_ mapGLPt2 ps

drawPoints :: Poly2-> IO ()
drawPoints (Poly2 p) = renderPrimitive Points p

drawPoly :: Poly2 -> IO ()
drawPoly (Poly2 p) = renderPrimitive LineLoop p

fillPoly :: Poly2 -> IO ()
fillPoly (Poly2 p) = renderPrimitive Polygon p

drawLine :: Poly2 -> IO ()
drawLine (Poly2 p) = renderPrimitive LineStrip p

drawColor :: RgbType -> RgbType -> RgbType -> IO ()
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

moveTo :: Pt2 Coord -> IO ()
moveTo = translate . ptVector3

innerDrawing :: IO () -> IO ()
innerDrawing = preservingMatrix

type RgbType = GLfloat
newtype PixelColor = PixelColor (RgbType,RgbType,RgbType)
  deriving (Eq,Show,Ord)

pixelColor :: RgbType -> RgbType -> RgbType -> PixelColor
pixelColor r g b = PixelColor (r,g,b)

pixelR, pixelG, pixelB :: PixelColor -> RgbType
pixelR (PixelColor (r,_,_)) = r
pixelG (PixelColor (_,g,_)) = g
pixelB (PixelColor (_,_,b)) = b

whitePixel, blackPixel :: PixelColor
whitePixel = pixelColor 1 1 1
blackPixel = pixelColor 0 0 0

instance Drawable PixelColor where
  draw (PixelColor (r,g,b)) = drawColor r g b
