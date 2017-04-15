module Main (
    main
) where

import Bindings
import Data.IORef
import Graphics.UI.GLUT

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12),cos (2*pi*k/12), 0) | k <- [1..12]]

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Hello World"
    reshapeCallback $= Just reshape
    depthFunc $= Just Less
    angle <- newIORef 0.0
    delta <- newIORef 0.1
    pos <- newIORef (0, 0)
    anum <- newIORef 1
    keyboardMouseCallback $= Just (keyboardMouse delta pos anum)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos anum
    mainLoop
