module Asteroids.GameLogic.KeyAction
  ( GameAction(..)
  , KeyAction(..)
  , keyActionName
  , allKeyActions
  , thrustAction
  , rotateLeftAction
  , rotateRightAction
  ) where

import Asteroids.GameLogic.Game
data KeyAction =
  Unknown
  | ToggleFullScreen
  | ExitGame
  | TurnLeft
  | TurnRight
  | Thrust
  | Fire
  deriving ( Show, Enum )


data GameAction = GameAction
  { gameActionName :: String
  , startAction :: Game -> Game
  , endAction :: Game -> Game }

instance Show GameAction where
  show = gameActionName

allKeyActions :: [KeyAction]
allKeyActions = [ Unknown ..]

keyActionName :: KeyAction -> String

keyActionName ToggleFullScreen = "Toggle FullScreen"
keyActionName ExitGame         = "Exit Game"
keyActionName k                = "(Unknown:" ++ show k ++ ")"


thrustAction :: GameAction
thrustAction = GameAction
  { gameActionName = "Thrust"
  , startAction = modifyShip $ setShipThrust True
  , endAction = modifyShip $ setShipThrust False }

rotateLeftAction :: GameAction
rotateLeftAction = GameAction
  { gameActionName = "Turn Left"
  , startAction = modifyShip $ setShipRotation TurnShipLeft
  , endAction = modifyShip $ setShipRotation StopShipTurning }

rotateRightAction :: GameAction
rotateRightAction = GameAction
  { gameActionName = "Turn Right"
  , startAction = modifyShip $ setShipRotation TurnShipRight
  , endAction = modifyShip $ setShipRotation StopShipTurning }

