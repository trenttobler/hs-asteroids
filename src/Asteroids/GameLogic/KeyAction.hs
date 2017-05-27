module Asteroids.GameLogic.KeyAction
  ( GameAction(..)
  , KeyAction(..)
  , applyGameAction, actionName
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
  | InsertCoin
  deriving ( Show, Enum, Eq )


data GameAction = GameAction
  { keyAction :: KeyAction
  , gameActionName :: String
  , startAction :: Game -> Game
  , endAction :: Game -> Game }

instance Show GameAction where
  show = gameActionName

allGameActions :: [GameAction]
allGameActions = [ thrustAction
               , rotateLeftAction
               , rotateRightAction
               , coinInsertedAction ]

actionName :: KeyAction -> String
actionName k = gameActionName' $ findAction k
  where gameActionName' Nothing = "(Not Implemented)"
        gameActionName' (Just ga) = gameActionName ga

findAction :: KeyAction -> Maybe GameAction
findAction k = found' ks
  where ks = filter (\x -> keyAction x == k) allGameActions
        found' []     = Nothing
        found' (x:_)  = Just x

applyGameAction :: KeyAction -> (GameAction -> Game -> Game) -> Game -> Game
applyGameAction k startOrEnd = applyGameAction' $ findAction k
  where applyGameAction' Nothing = id
        applyGameAction' (Just ga) = startOrEnd ga

thrustAction :: GameAction
thrustAction = GameAction
  { keyAction = Thrust
  , gameActionName = "Thrust"
  , startAction = modifyShip $ setShipThrust True
  , endAction = modifyShip $ setShipThrust False }

rotateLeftAction :: GameAction
rotateLeftAction = GameAction
  { keyAction = TurnLeft
  , gameActionName = "Turn Left"
  , startAction = modifyShip $ setShipRotation TurnShipLeft
  , endAction = modifyShip $ setShipRotation StopShipTurning }

rotateRightAction :: GameAction
rotateRightAction = GameAction
  { keyAction = TurnRight
  , gameActionName = "Turn Right"
  , startAction = modifyShip $ setShipRotation TurnShipRight
  , endAction = modifyShip $ setShipRotation StopShipTurning }

coinInsertedAction :: GameAction
coinInsertedAction = GameAction
  { keyAction = InsertCoin
  , gameActionName = "Insert Coin"
  , startAction = coinInserted
  , endAction = id }
