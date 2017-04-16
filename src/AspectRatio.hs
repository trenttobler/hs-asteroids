module AspectRatio (
  AspectRatio,
  newAspectRatio,
  adjustAspectRatio,
  obscureAspectRatio,
  defaultAspectRatio
) where

import Graphics.UI.GLUT
import GLConverters
import Shapes

newtype AspectRatio = AspectRatio (IO (), IO ())

defaultAspectRatio :: AspectRatio
defaultAspectRatio = newAspectRatio (Size 800 600)

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

newAspectRatio :: Size -> AspectRatio
newAspectRatio (Size xx yy) = AspectRatio (ratio xx yy,obscureBorders)
  where
    ratio = if xx < yy then tallAspectRatio else wideAspectRatio

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


adjustAspectRatio :: AspectRatio -> IO ()
adjustAspectRatio (AspectRatio (a,_)) = a

obscureAspectRatio :: AspectRatio -> IO ()
obscureAspectRatio (AspectRatio (_,o)) = o