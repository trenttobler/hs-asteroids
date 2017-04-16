module Bindings (
  idle,
  display,
  reshape,
  keyboardMouse,
) where

import           Display
import           GameState
import           Graphics.UI.GLUT
import           System.Exit

reshape :: GameState -> ReshapeCallback
reshape state size = do
  setAspectRatioSize state size
  viewport $= (Position 0 0, size)

keyboardMouse :: GameState -> KeyboardMouseCallback
keyboardMouse _ key Down _ _ = case key of
  (SpecialKey KeyF2 ) -> fullScreen
  (SpecialKey KeyF3 ) -> windowSize $= Size 640 400
  (Char '\ESC') -> do
    putStrLn "Exiting"
    exitSuccess
  _ -> return ()

keyboardMouse _ _ _ _ _ = return ()
