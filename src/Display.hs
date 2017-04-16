module Display (
  display,
  idle
) where

import           Control.Monad
import           Data.IORef
import           Game
import           GameState
import           Graphics.UI.GLUT
import           Shapes

display :: GameState -> DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  adjustAspectRatio state
  game <- getGame state
  forM_ (getEntities game) drawGL
  obscureAspectRatio state
  swapBuffers

idle :: GameState -> IdleCallback
idle state = do
  let dt = 0.001 -- use high precision clock / time
  updateGame state dt
  postRedisplay Nothing

