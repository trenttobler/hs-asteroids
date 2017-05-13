module Asteroids.GameLogic.Game (
  newGame,
  Game(..),
  gameStep,
  getEntities
)
where

import           Entity
import           Pt2
import           Rand
import           Utils

newtype Game = Game [Entity]

getEntities :: Game -> [Entity]
getEntities (Game es) = es

entities::[Entity]
entities = makeShip (Pt2 (0,0)) 0
           : getSeedRand 1 (sequence [ makeAsteroid (0.1000/fromIntegral (s `mod` 10)) s | s <- [1..40] ])

newGame :: IO Game
newGame = return $ Game entities

gameStep :: Coord -> ShipState -> Game -> Game
gameStep dt ship (Game e) = Game $ fmap (entityStep dt ship) e
