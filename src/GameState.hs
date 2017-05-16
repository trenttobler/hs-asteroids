module GameState (
  Drawable(..),
  GameState,
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

import           Asteroids.GameLogic.Game
import           Asteroids.GameLogic.GameOptions
import           Asteroids.GameLogic.KeyAction
import           Asteroids.UILogic.AspectRatio
import           Asteroids.UILogic.Drawable
import           Asteroids.UILogic.KeyBindings
import           Control.Monad
import           Data.IORef
import           Data.Time
import           Graphics.UI.GLUT
import           System.Exit

data PlayStatus =
  StartingGame
  | PlayingGame
  deriving ( Enum, Eq, Show )

-- Need change this to a single IORef with nested game state...
-- type GameStateRef = (IORef GameState)
--
data GameState = GameState {
  aspectRef      :: IORef AspectRatio,
  gameRef        :: IORef Game,
  optionsRef     :: IORef GameOptions,
  keyBindingsRef :: IORef KeyBindings,
  lastTimeRef    :: IORef UTCTime,
  _statusRef     :: IORef PlayStatus }

instance Drawable GameState where
  draw state = do
    game <- getGame state
    draw game

newGameState :: Int -> Int -> IO GameState
newGameState seed level = do
  currentTime <- getCurrentTime
  let game = newGame seed level
  aRef <- newIORef defaultAspectRatio
  gRef <- newIORef game
  oRef <- newIORef defaultGameOptions
  kRef <- newIORef defaultKeyBindings
  cRef <- newIORef currentTime
  stRef <- newIORef StartingGame
  return $ GameState aRef gRef oRef kRef cRef stRef

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

setAspectRatioSize :: GameState -> Size -> IO ()
setAspectRatioSize state size = setAspectRatio state $ aspectRatio size

updateGame::GameState -> IO ()
updateGame state = do
  lastTime <- getLastTime state
  curTime <- getCurrentTime
  setLastTime state curTime
  let dt = realToFrac $ diffUTCTime curTime lastTime
  game <- getGame state
  let game' = gameStep dt game
  setGame state game'

restoreScreen :: GameState -> IO ()
restoreScreen state = do
  options <- getOptions state
  if hasOption options FullScreenOption
    then fullScreen
    else windowSize $= Size 640 400

-- actions that can be performed via key mappings

performAction :: KeyAction -> GameState -> KeyState -> IO ()
performAction ToggleFullScreen = downOnlyAction fullScreenAction
performAction Thrust = performGameAction thrustAction
performAction TurnLeft = performGameAction rotateLeftAction
performAction TurnRight = performGameAction rotateRightAction
performAction ExitGame = alwaysAction exitSuccess
performAction Unknown = alwaysAction noAction
performAction a = downOnlyAction logUnknown
  where logUnknown _ = putStrLn $ "Action not implemented: " ++ show a

gameKeyAction :: KeyState -> (GameAction -> (Game->Game))
gameKeyAction Down = startAction
gameKeyAction Up   = endAction

performGameAction :: GameAction -> GameState -> KeyState -> IO ()
performGameAction gameAction state upOrDown = do
  game <- getGame state
  setGame state $ gameKeyAction upOrDown gameAction game

fullScreenAction :: GameState -> IO ()
fullScreenAction state = do
  opts <- getOptions state
  setOptions state (toggleOption opts FullScreenOption)
  restoreScreen state

downOnlyAction :: (GameState -> IO ()) -> GameState -> KeyState -> IO ()
downOnlyAction f state upOrDown = when (upOrDown == Down) $ f state

alwaysAction :: IO () -> GameState -> KeyState -> IO ()
alwaysAction a _ _ = a

noAction :: IO ()
noAction = return ()