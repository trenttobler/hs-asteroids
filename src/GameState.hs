module GameState (
  GameState,
  adjustAspectRatio,
  obscureAspectRatio,
  defaultGameState,
  setAspectRatioSize,
  newGameState,
  getGame,
  updateGame
) where

import           Data.IORef
import           Game
import           GLConverters
import           Graphics.UI.GLUT
import           Shapes

newtype GameState = GameState (IORef AspectRatio, IORef Game)

newGameState :: IO GameState
newGameState = do
  aspectRef <- newIORef defaultAspectRatio
  gameRef <- newIORef newGame
  return $ GameState (aspectRef,gameRef)

getGame::GameState -> IO Game
getGame (GameState (_,gref)) = readIORef gref

setGame::GameState -> Game -> IO ()
setGame (GameState (_,gref)) = writeIORef gref

updateGame::GameState -> Double -> IO ()
updateGame state dt = do
  game <- getGame state
  setGame state (gameStep dt game)

getAspectRatio :: GameState -> IO AspectRatio
getAspectRatio (GameState (aref,_)) = readIORef aref

setAspectRatio :: GameState -> AspectRatio -> IO ()
setAspectRatio (GameState (aref,_)) a = do
  writeIORef aref a
  return ()

setAspectRatioSize :: GameState -> Size -> IO ()
setAspectRatioSize state size = setAspectRatio state ratio
  where ratio = AspectRatio size

newtype AspectRatio = AspectRatio Size

defaultAspectRatio :: AspectRatio
defaultAspectRatio = AspectRatio (Size 800 600)

defaultGameState :: IO GameState
defaultGameState = do
  ratio <- newIORef defaultAspectRatio
  game <- newIORef newGame
  let state = GameState (ratio, game)
  return state

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
