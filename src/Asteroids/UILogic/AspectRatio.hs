module Asteroids.UILogic.AspectRatio (
    AspectRatio(..),
    adjustAspectRatio,
    defaultAspectRatio,
    obscureBorders
) where

import           Graphics.UI.GLUT
import           Pt2
import           Utils

newtype AspectRatio = AspectRatio Size

defaultAspectRatio :: AspectRatio
defaultAspectRatio = AspectRatio (Size 800 600)

adjustAspectRatio :: AspectRatio -> IO ()
adjustAspectRatio (AspectRatio (Size xx yy)) =
    if xx < yy
      then tallAspectRatio xx yy
      else wideAspectRatio xx yy

tallAspectRatio :: GLsizei -> GLsizei -> IO ()
tallAspectRatio xx yy = adjusted
  where
    x = fromIntegral xx
    y = fromIntegral yy
    d = x / y :: GLfloat
    adjusted = scale 1 d 1

wideAspectRatio :: GLsizei -> GLsizei -> IO ()
wideAspectRatio xx yy = adjusted
  where
    x = fromIntegral xx
    y = fromIntegral yy
    d = y / x :: GLfloat
    adjusted = scale d 1 1

obscureBorders :: IO ()
obscureBorders = do
    renderPrimitive LineLoop $ mapGLPt2s border
    renderPrimitive Polygon  $ mapGLPt2s fillLt
    renderPrimitive Polygon  $ mapGLPt2s fillRt
    renderPrimitive Polygon  $ mapGLPt2s fillUp
    renderPrimitive Polygon  $ mapGLPt2s fillDn
  where
    m = 10
    border = [Pt2 (-0.99,-0.99), Pt2 (-0.99, 0.99), Pt2 ( 0.99, 0.99), Pt2 ( 0.99,-0.99)]
    fillLt = [Pt2 (-m,-m), Pt2 (-m, m), Pt2 (-0.99, m), Pt2 (-0.99,-m)]
    fillRt = [Pt2 ( m,-m), Pt2 ( m, m), Pt2 ( 0.99, m), Pt2 ( 0.99,-m)]
    fillUp = [Pt2 (-m,-m), Pt2 ( m,-m), Pt2 ( m,-0.99), Pt2 (-m,-0.99)]
    fillDn = [Pt2 (-m, m), Pt2 ( m, m), Pt2 ( m, 0.99), Pt2 (-m, 0.99)]
