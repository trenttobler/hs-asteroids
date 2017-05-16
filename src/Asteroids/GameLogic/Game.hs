module Asteroids.GameLogic.Game (
  module Asteroids.GameLogic.Asteroid,
  module Asteroids.GameLogic.Ship,
  Game(..),
  newGame,
  gameStep,
  modifyShip
)
where

import           Asteroids.GameLogic.Asteroid
import           Asteroids.GameLogic.Ship
import           Asteroids.UILogic.Drawable
import           Rand

data Game = Game
  { asteroids :: [Asteroid]
  , gameSeed  :: Int
  , gameLevel :: Int
  , ship      :: Ship }

instance Drawable Game where
  draw game = do
    draw $ ship game
    mapM_ draw $ asteroids game

newGame :: Int -> Int -> Game
newGame seed level = Game
  { asteroids = runSeedRand seed $ createRandomAsteroids level
  , gameSeed = seed
  , gameLevel = level
  , ship = createShip (pt2 0 0) 0 }

createRandomAsteroids :: Int -> RandomState [Asteroid]
createRandomAsteroids level =
  sequence [ createRandomAsteroid (sz s) s | s <- [1..level] ]
  where sz s = 0.1000 / fromIntegral (s `mod` 10)

gameStep :: TimeDelta -> Game -> Game
gameStep dt game = game'
  where asteroids' = fmap (asteroidStep dt) (asteroids game)
        ship' = shipStep dt (ship game)
        game' = game { asteroids = asteroids', ship = ship' }

modifyShip :: (Ship->Ship) -> Game -> Game
modifyShip f game = game { ship = f $ ship game }
