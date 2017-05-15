module Asteroids.GameLogic.Game (
  Game(..),
  newGame,
  gameStep
)
where

import           Asteroids.GameLogic.Entity
import           Asteroids.GameLogic.Ship
import           Asteroids.UILogic.Drawable
import           Pt2
import           Rand

data Game = Game
  { game'Entities :: [Entity]
  , game'Seed :: Int
  , game'Level :: Int }

instance Drawable Game where
  draw game = mapM_ draw (game'Entities game)
        
newGame :: Int -> Int -> IO Game
newGame seed level = let
  ship = makeShip (Pt2 (0,0)) 0
  asteroids = runSeedRand seed (sequence [ makeAsteroid (0.1000/fromIntegral (s `mod` 10)) s | s <- [1..level] ])
  game = Game (ship:asteroids) seed level
  in return game

gameStep :: Coord -> ShipState -> Game -> Game
gameStep dt ship g = g { game'Entities = es }
  where es = fmap step (game'Entities g)
        step e = (entity'Step e) dt ship
