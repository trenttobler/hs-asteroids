module Asteroids.UILogic.Display (
  display,
  idle
) where

import           Control.Concurrent
import           GameState
import           Graphics.UI.GLUT

display :: GameState -> DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  aspect <- getAspectRatio state
  adjustAspectRatio aspect
  draw state
  obscureAspectRatio state
  swapBuffers

idle :: GameState -> IdleCallback
idle state = do
  threadDelay 20000
  updateGame state
  postRedisplay Nothing
