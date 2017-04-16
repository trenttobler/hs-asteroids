module GameState (
  GameState,
  adjustAspectRatio,
  obscureAspectRatio,
  setAspectRatioSize,
  newGameState,
  getGame,
  updateGame,
  getOptions,
  setOptions,
  performAction,
  getKeyBindings,
  setKeyBindings,
  restoreScreen
) where

import           Data.IORef
import           Game
import           GameOptions
import           GLConverters
import           Graphics.UI.GLUT
import           KeyAction
import           KeyBindings
import           Shapes
import           System.Exit
import           System.IO

-- Need to learn how to do lenses so this is easier to do.
newtype GameState = GameState (
  IORef AspectRatio,
  IORef Game,
  IORef GameOptions,
  IORef KeyBindings )

getKeyBindings :: GameState -> IO KeyBindings
getKeyBindings (GameState (_,_,_,obind)) = readIORef obind

setKeyBindings :: GameState -> KeyBindings -> IO ()
setKeyBindings (GameState (_,_,_,obind)) = writeIORef obind

getOptions :: GameState -> IO GameOptions
getOptions (GameState (_,_,oref,_)) = readIORef oref

setOptions :: GameState -> GameOptions -> IO ()
setOptions (GameState (_,_,oref,_)) = writeIORef oref

newGameState :: IO GameState
newGameState = do
  aspectRef <- newIORef defaultAspectRatio
  gameRef <- newIORef newGame
  options <- newIORef defaultGameOptions
  keyBindings <- newIORef defaultKeyBindings
  return $ GameState (aspectRef,gameRef,options, keyBindings)

getGame::GameState -> IO Game
getGame (GameState (_,gref,_,_)) = readIORef gref

setGame::GameState -> Game -> IO ()
setGame (GameState (_,gref,_,_)) = writeIORef gref

updateGame::GameState -> Double -> IO ()
updateGame state dt = do
  game <- getGame state
  setGame state (gameStep dt game)

getAspectRatio :: GameState -> IO AspectRatio
getAspectRatio (GameState (aref,_,_,_)) = readIORef aref

setAspectRatio :: GameState -> AspectRatio -> IO ()
setAspectRatio (GameState (aref,_,_,_)) a = do
  writeIORef aref a
  return ()

setAspectRatioSize :: GameState -> Size -> IO ()
setAspectRatioSize state size = setAspectRatio state ratio
  where ratio = AspectRatio size

newtype AspectRatio = AspectRatio Size

defaultAspectRatio :: AspectRatio
defaultAspectRatio = AspectRatio (Size 800 600)

tallAspectRatio :: GLsizei -> GLsizei -> IO ()
tallAspectRatio xx yy = adjusted
  where
    x = fromIntegral xx
    y = fromIntegral yy
    d = x / y :: GLfloat
    adjusted = scale 1 d 1

wideAspectRatio :: GLsizei -> GLsizei -> IO ()
wideAspectRatio xx yy = adjusted
  where
    x = fromIntegral xx
    y = fromIntegral yy
    d = y / x :: GLfloat
    adjusted = scale d 1 1

obscureBorders :: IO ()
obscureBorders = do
    renderPrimitive LineLoop $ mapGLPt2s border
    renderPrimitive Polygon  $ mapGLPt2s fillLt
    renderPrimitive Polygon  $ mapGLPt2s fillRt
    renderPrimitive Polygon  $ mapGLPt2s fillUp
    renderPrimitive Polygon  $ mapGLPt2s fillDn
  where
    m = 10
    border = [Pt2 (-0.99,-0.99), Pt2 (-0.99, 0.99), Pt2 ( 0.99, 0.99), Pt2 ( 0.99,-0.99)]
    fillLt = [Pt2 (-m,-m), Pt2 (-m, m), Pt2 (-0.99, m), Pt2 (-0.99,-m)]
    fillRt = [Pt2 ( m,-m), Pt2 ( m, m), Pt2 ( 0.99, m), Pt2 ( 0.99,-m)]
    fillUp = [Pt2 (-m,-m), Pt2 ( m,-m), Pt2 ( m,-0.99), Pt2 (-m,-0.99)]
    fillDn = [Pt2 (-m, m), Pt2 ( m, m), Pt2 ( m, 0.99), Pt2 (-m, 0.99)]

adjustAspectRatio :: GameState -> IO ()
adjustAspectRatio state = do
   (AspectRatio (Size xx yy )) <- getAspectRatio state
   if xx < yy
     then tallAspectRatio xx yy
     else wideAspectRatio xx yy

obscureAspectRatio :: GameState -> IO ()
obscureAspectRatio _ = obscureBorders

restoreScreen :: GameState -> IO ()
restoreScreen state = do
  options <- getOptions state
  if hasOption options FullScreenOption
    then fullScreen
    else windowSize $= Size 640 400

-- actions that can be performed via key mappings

performAction :: KeyAction -> GameState -> IO ()
performAction ToggleFullScreen state = do
  options <- getOptions state
  setOptions state (toggleOption options FullScreenOption)
  restoreScreen state

performAction ExitGame _ = exitSuccess

performAction Unknown _ = return ()

performAction a _ = do
  putStrLn $ "Action not implemented: " ++ show a
  hFlush stdout
