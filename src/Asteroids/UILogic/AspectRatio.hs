module Asteroids.UILogic.AspectRatio (
    AspectRatio(..),
    adjustAspectRatio,
    defaultAspectRatio,
    obscureBorders
) where

import           Asteroids.UILogic.Drawable
import           Graphics.UI.GLUT
import           Pt2

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
    drawPoly $ pt2ToPoly border
    fillPoly $ pt2ToPoly fillLt
    fillPoly $ pt2ToPoly fillRt
    fillPoly $ pt2ToPoly fillUp
    fillPoly $ pt2ToPoly fillDn
  where
    m = 10
    border = [Pt2 (-0.99,-0.99), Pt2 (-0.99, 0.99), Pt2 ( 0.99, 0.99), Pt2 ( 0.99,-0.99)]
    fillLt = [Pt2 (-m,-m), Pt2 (-m, m), Pt2 (-0.99, m), Pt2 (-0.99,-m)]
    fillRt = [Pt2 ( m,-m), Pt2 ( m, m), Pt2 ( 0.99, m), Pt2 ( 0.99,-m)]
    fillUp = [Pt2 (-m,-m), Pt2 ( m,-m), Pt2 ( m,-0.99), Pt2 (-m,-0.99)]
    fillDn = [Pt2 (-m, m), Pt2 ( m, m), Pt2 ( m, 0.99), Pt2 (-m, 0.99)]
