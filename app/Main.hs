module Main (
    main
) where

import Bindings
import Data.IORef
import Graphics.UI.GLUT

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Haskell OpenGL Asteroids"
    angle <- newIORef 0.0
    delta <- newIORef 0.1
    pos <- newIORef (0, 0)
    size <- newIORef (Size 100 100)
    reshapeCallback $= Just ( reshape size )
    depthFunc $= Just Less
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos size
    mainLoop
