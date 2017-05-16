module Asteroids.UILogic.AspectRatio (
    AspectRatio,
    aspectRatio,
    adjustAspectRatio,
    defaultAspectRatio,
    computeAspectRatio,
    obscureBorders
) where

import           Asteroids.UILogic.Drawable
import           Graphics.UI.GLUT

data AspectRatio = AspectRatio
  { aspectSize :: Size
  , obscureBorders :: IO () }

aspectRatio :: Size -> AspectRatio
aspectRatio size = AspectRatio { aspectSize = size,
                                 obscureBorders = obscureBorders' }

defaultAspectRatio :: AspectRatio
defaultAspectRatio = aspectRatio (Size 800 600)

adjustAspectRatio :: AspectRatio -> IO ()
adjustAspectRatio aspect = let
  (x,y) = computeAspectRatio aspect
  (n,p) = (-1,1)
  in do
    ortho n n n p p p
    scale x y 1

computeAspectRatio :: AspectRatio -> (GLfloat, GLfloat)
computeAspectRatio aspect = let
  (Size xx yy) = aspectSize aspect
  in if xx < yy
    then computeTallAspectRatio xx yy
    else computeWideAspectRatio xx yy

computeTallAspectRatio :: (Fractional t, Integral s) => s -> s -> (t, t)
computeTallAspectRatio xx yy = adjusted
  where
    x = fromIntegral xx
    y = fromIntegral yy
    d = x / y
    adjusted = (1,d)

computeWideAspectRatio :: (Fractional t, Integral s) => s -> s -> (t, t)
computeWideAspectRatio xx yy = adjusted
  where
    x = fromIntegral xx
    y = fromIntegral yy
    d = y / x
    adjusted = (d,1)

obscureBorders' :: IO ()
obscureBorders' = let
  (x,y) = (0.9,0.9) -- fmap (*0.99) $ computeAspectRatio aspect
  m = 10
  borderLine = pt2ToPoly $ fmap Pt2 borderPts
  borderFill = fmap (pt2ToPoly . fmap Pt2) borderPolyPts
  borderPts =       [(-x,-y), (-x, y), ( x, y), ( x,-y)]
  borderPolyPts = [ [(-m,-m), (-m, m), (-x, m), (-x,-m)]
                  , [( m,-m), ( m, m), ( x, m), ( x,-m)]
                  , [(-m,-m), ( m,-m), ( m,-y), (-m,-y)]
                  , [(-m, m), ( m, m), ( m, y), (-m, y)] ]
  in do
    drawColor 0.5 1 0.5
    mapM_ fillPoly borderFill
    drawColor 0 1 1
    drawPoly borderLine
