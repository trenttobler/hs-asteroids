module Bindings (
  idle,
  display,
  reshape,
  keyboardMouse
) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
import GLConverters
import System.Exit
 
reshape :: IORef Size -> ReshapeCallback
reshape sizeRef size = do
  writeIORef sizeRef size
  viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (mod2pi . (* 2))
  (Char '-') -> a $~! (mod2pi . (/ 2))
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyF2 ) -> fullScreen
  (SpecialKey KeyF3 ) -> windowSize $= Size 300 300
  (Char 'x') -> do
    putStrLn "Exiting"
    exitSuccess
  _ -> return ()

keyboardMouse _ _ _ _ _ _ = return ()
