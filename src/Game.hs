module Game (
  newGame,
  GameState(..),
  gameStep
)
where

import Entity
import Shapes

newtype GameState = GameState [Entity]

entities::[Entity]
entities = makeShip (Pt2 (0,0)) 0
           : [ makeAsteroid (0.1000/fromIntegral s) s | s <- [1..10] ]

newGame :: GameState
newGame = GameState entities


gameStep :: Double -> GameState -> GameState
gameStep dt (GameState e)=
    GameState $ fmap (entityStep dt) e
