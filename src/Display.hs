module Display (
  display,
  idle
) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Shapes
import Game
import AspectRatio

display :: IORef GameState -> IORef AspectRatio -> DisplayCallback
display gameState aspectRatio = do
  clear [ColorBuffer, DepthBuffer]
  aspect <- readIORef aspectRatio
  loadIdentity
  adjustAspectRatio aspect
  (GameState entities) <- readIORef gameState
  forM_ entities drawGL
  obscureAspectRatio aspect
  swapBuffers

idle :: IORef GameState -> IdleCallback
idle gameState = do
  oldState <- readIORef gameState
  let dt = 0.001 -- use high precision clock / time
  let newState = gameStep dt oldState
  writeIORef gameState newState
  postRedisplay Nothing

