module Bindings (
  idle,
  display,
  reshape,
  keyboardMouse
) where
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Initialization
import Data.IORef
import Display
import GLConverters
import System.Exit
 
reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef Int -> KeyboardMouseCallback
keyboardMouse a p n key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (mod2pi . (* 2))
  (Char '-') -> a $~! (mod2pi . (/ 2))
  (Char '>') -> n $~! (+ 1)
  (Char '<') -> n $~! (+ (-1))
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyF2 ) -> fullScreen
  (SpecialKey KeyF3 ) -> windowSize $= Size 300 300
  (Char 'x') -> do
    putStrLn "Exiting"
    exitSuccess
  _ -> return ()

keyboardMouse _ _ _ _ _ _ _ = return ()
