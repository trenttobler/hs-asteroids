module Asteroids.UILogic.Display (
  display,
  idle
) where

import           Asteroids.UILogic.AspectRatio
import           Asteroids.UILogic.Drawable
import           Control.Concurrent
import           GameState
import           Graphics.Rendering.OpenGL.GL.CoordTrans
import           Graphics.UI.GLUT

display :: GameState -> DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  aspect <- getAspectRatio state
  adjustAspectRatio aspect
  innerDrawing $ obscureBorders aspect
  innerDrawing $ draw state
  swapBuffers

idle :: GameState -> IdleCallback
idle state = do
  threadDelay 20000
  updateGame state
  postRedisplay Nothing
