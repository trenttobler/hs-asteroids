module Game (
  newGame,
  Game(..),
  gameStep,
  getEntities
)
where

import           Entity
import           Shapes

newtype Game = Game [Entity]

getEntities :: Game -> [Entity]
getEntities (Game es) = es

entities::[Entity]
entities = makeShip (Pt2 (0,0)) 0
           : [ makeAsteroid (0.1000/fromIntegral s) s | s <- [1..10] ]

newGame :: Game
newGame = Game entities

gameStep :: Double -> Game -> Game
gameStep dt (Game e)=
    Game $ fmap (entityStep dt) e
