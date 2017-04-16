module Main (
    main
) where

import Game
import Bindings
import Data.IORef
import Graphics.UI.GLUT
import AspectRatio

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Haskell OpenGL Asteroids"
    game <- newIORef newGame
    aspectRatio <- newIORef defaultAspectRatio
    reshapeCallback $= Just (reshape aspectRatio)
    depthFunc $= Just Less
    keyboardMouseCallback $= Just (keyboardMouse game)
    idleCallback $= Just (idle game)
    displayCallback $= display game aspectRatio
    mainLoop
