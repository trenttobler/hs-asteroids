module Display (
  display,
  idle
) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Shapes
import Asteroid
import GLConverters
import Rand

black = Color3 (0::GLfloat) 0 0
acolor = Color3 (0.5::GLfloat) 0.7 1.0

posAsters::[(GLfloat,GLfloat,Asteroid)]
posAsters = [
  ( 0.0, 0.0,  asteroid 0 0.100),
  ( 0.6, 0.6,  asteroid 1 0.100),
  (-0.9, 0.9,  asteroid 2 0.050),
  (-0.5,-0.5,  asteroid 3 0.025) ]

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef Int -> DisplayCallback
display angle pos n = do 
  anum <- get n
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0 1
    scale 0.7 0.7 (0.7::GLfloat)

    forM_ posAsters $ \(x,y,aa) ->
      preservingMatrix $ do
        translate $ Vector3 x y 0
        drawGL aa

    swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (mod2pi . (+ d))
  postRedisplay Nothing