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
  restoreScreen,
  getAspectRatio
) where

import           Asteroids.UILogic.AspectRatio
import           Data.IORef
import           Data.Time
import           Entity
import           Asteroids.GameLogic.Game
import           Asteroids.GameLogic.GameOptions
import           Graphics.UI.GLUT
import           Asteroids.GameLogic.KeyAction
import           KeyBindings
import           System.Exit
import           System.IO


data PlayStatus =
  StartingGame
  | PlayingGame
  deriving ( Enum, Eq, Show )

-- Need to learn how to do lenses so this is easier to do.
data GameState = GameState {
  aspectRef      :: IORef AspectRatio,
  gameRef        :: IORef Game,
  optionsRef     :: IORef GameOptions,
  keyBindingsRef :: IORef KeyBindings,
  lastTimeRef    :: IORef UTCTime,
  statusRef      :: IORef PlayStatus,
  shipRef        :: IORef ShipState }

newGameState :: IO GameState
newGameState = do
  currentTime <- getCurrentTime
  game <- newGame
  aRef <- newIORef defaultAspectRatio
  gRef <- newIORef game
  oRef <- newIORef defaultGameOptions
  kRef <- newIORef defaultKeyBindings
  cRef <- newIORef currentTime
  stRef <- newIORef StartingGame
  shRef <- newIORef initialShipState
  return $ GameState aRef gRef oRef kRef cRef stRef shRef

getIO :: (GameState -> IORef a) -> GameState -> IO a
getIO prop state = readIORef (prop state)

setIO :: (GameState -> IORef a) -> GameState -> a -> IO ()
setIO prop state = writeIORef (prop state)

getKeyBindings :: GameState -> IO KeyBindings
getKeyBindings  = getIO keyBindingsRef
setKeyBindings :: GameState -> KeyBindings -> IO ()
setKeyBindings  = setIO keyBindingsRef

getOptions :: GameState -> IO GameOptions
getOptions      = getIO optionsRef
setOptions :: GameState -> GameOptions -> IO ()
setOptions      = setIO optionsRef

getGame :: GameState -> IO Game
getGame         = getIO gameRef
setGame :: GameState -> Game -> IO ()
setGame         = setIO gameRef

getAspectRatio :: GameState -> IO AspectRatio
getAspectRatio  = getIO aspectRef
setAspectRatio :: GameState -> AspectRatio -> IO ()
setAspectRatio  = setIO aspectRef

getLastTime :: GameState -> IO UTCTime
getLastTime    = getIO lastTimeRef
setLastTime :: GameState -> UTCTime -> IO ()
setLastTime    = setIO lastTimeRef

getShip :: GameState -> IO ShipState
getShip = getIO shipRef
setShip :: GameState -> ShipState -> IO ()
setShip = setIO shipRef

setAspectRatioSize :: GameState -> Size -> IO ()
setAspectRatioSize state size = setAspectRatio state $ AspectRatio size

updateGame::GameState -> IO ()
updateGame state = do
  lastTime <- getLastTime state
  curTime <- getCurrentTime
  setLastTime state curTime
  let dt = realToFrac $ diffUTCTime curTime lastTime
  game <- getGame state
  ship <- getShip state
  setGame state (gameStep dt ship game)

obscureAspectRatio :: GameState -> IO ()
obscureAspectRatio _ = obscureBorders

restoreScreen :: GameState -> IO ()
restoreScreen state = do
  options <- getOptions state
  if hasOption options FullScreenOption
    then fullScreen
    else windowSize $= Size 640 400

-- actions that can be performed via key mappings

performAction :: KeyAction -> GameState -> KeyState -> IO ()
performAction ToggleFullScreen state Down = do
  options <- getOptions state
  setOptions state (toggleOption options FullScreenOption)
  restoreScreen state

performAction TurnLeft state upOrDown = performShipTurn state
  $ shipRotate TurnShipLeft upOrDown

performAction TurnRight state upOrDown = performShipTurn state
  $ shipRotate TurnShipRight upOrDown

performAction Thrust state upOrDown = do
    ship <- getShip state
    setShip state $ ship { shipThrusting = thrusting }
  where thrusting = upOrDown == Down

performAction ExitGame _ _ = exitSuccess

performAction Unknown _ _ = return ()

performAction _ _ Up = return ()

performAction a _ _ = do
  putStrLn $ "Action not implemented: " ++ show a
  hFlush stdout

shipRotate :: ShipRotation -> KeyState -> ShipRotation
shipRotate dir Down = dir
shipRotate _ Up     = StopShipTurning

performShipTurn :: GameState -> ShipRotation -> IO ()
performShipTurn state dir = do
    ship <- getShip state
    setShip state $ ship { shipRotation = dir }

