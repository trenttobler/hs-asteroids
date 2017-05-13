module Asteroids.GameLogic.Game (
  newGame,
  Game(..),
  gameStep,
  getEntities
)
where

import           Entity
import           Pt2

newtype Game = Game [Entity]

getEntities :: Game -> [Entity]
getEntities (Game es) = es

entities::[Entity]
entities = makeShip (Pt2 (0,0)) 0
           : [ makeAsteroid (0.1000/fromIntegral s) s | s <- [1..10] ]

newGame :: IO Game
newGame = return $ Game entities

gameStep :: Double -> ShipState -> Game -> Game
gameStep dt ship (Game e)=
    Game $ fmap (entityStep dt ship) e
