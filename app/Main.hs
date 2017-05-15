module Main (
    main
) where

import           Asteroids
import           Graphics.UI.GLUT
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Haskell OpenGL Asteroids"
    state <- newGameState 1 10
    reshapeCallback $= Just (reshape state)
    depthFunc $= Just Less
    keyboardMouseCallback $= Just (keyboardMouse state)
    idleCallback $= Just (idle state)
    displayCallback $= display state
    restoreScreen state
    mainLoop
