module Asteroids.UILogic.Display (
  display,
  idle
) where

import           Control.Monad
import           Asteroids.GameLogic.Game
import           GameState
import           Graphics.UI.GLUT
import           Shapes

display :: GameState -> DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  aspect <- getAspectRatio state
  adjustAspectRatio aspect
  game <- getGame state
  forM_ (getEntities game) drawGL
  obscureAspectRatio state
  swapBuffers

idle :: GameState -> IdleCallback
idle state = do
  updateGame state
  postRedisplay Nothing

