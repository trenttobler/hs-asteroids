module Main (
    main
) where

import           Bindings
import           GameState
import           Graphics.UI.GLUT

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Haskell OpenGL Asteroids"
    state <- newGameState
    reshapeCallback $= Just (reshape state)
    depthFunc $= Just Less
    keyboardMouseCallback $= Just (keyboardMouse state)
    idleCallback $= Just (idle state)
    displayCallback $= display state
    restoreScreen state
    mainLoop
