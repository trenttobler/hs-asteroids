module Bindings (
  idle,
  display,
  reshape,
  keyboardMouse,
) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
import System.Exit
import Game
import AspectRatio

reshape :: IORef AspectRatio -> ReshapeCallback
reshape aspectRatio size =
  let
    adjustedRatio = newAspectRatio size
  in do
    writeIORef aspectRatio adjustedRatio
    viewport $= (Position 0 0, size)

keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse _ key Down _ _ = case key of
  (SpecialKey KeyF2 ) -> fullScreen
  (SpecialKey KeyF3 ) -> windowSize $= Size 300 300
  (Char '\ESC') -> do
    putStrLn "Exiting"
    exitSuccess
  _ -> return ()

keyboardMouse _ _ _ _ _ = return ()
