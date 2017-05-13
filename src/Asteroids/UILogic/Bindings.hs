module Asteroids.UILogic.Bindings (
  idle,
  display,
  reshape,
  keyboardMouse,
) where

import           Asteroids.UILogic.Display
import           GameState
import           Graphics.UI.GLUT
import           KeyBindings

reshape :: GameState -> ReshapeCallback
reshape state size = do
  setAspectRatioSize state size
  viewport $= (Position 0 0, size)

keyboardMouse :: GameState -> KeyboardMouseCallback
keyboardMouse state key keyState modifiers _ = do
    keyMap <- getKeyBindings state
    performAction (getAction keyMap key modifiers) state keyState
