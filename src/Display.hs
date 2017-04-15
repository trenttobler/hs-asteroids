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

posAsters::[(GLfloat,GLfloat,Asteroid)]
posAsters = [
  ( 0.0, 0.0,  asteroid 0 0.100),
  ( 0.6, 0.6,  asteroid 1 0.100),
  (-0.9, 0.9,  asteroid 2 0.050),
  (-0.5,-0.5,  asteroid 3 0.025),
  ( 1.0,-1.0,  asteroid 4 0.0125) ]

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef Size -> DisplayCallback
display angle pos size = do
  clear [ColorBuffer, DepthBuffer]
  Size dx dy <- readIORef size

  loadIdentity
  
  if dx < dy
    then scale 1 (fromIntegral dx / fromIntegral dy) (1.0::GLfloat)
    else scale (fromIntegral dy / fromIntegral dx) 1 (1.0::GLfloat)

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