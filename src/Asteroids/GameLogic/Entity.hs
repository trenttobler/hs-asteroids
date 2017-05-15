module Asteroids.GameLogic.Entity (
  Entity(..),
  Coord,
  makeAsteroid,
  makeShip,
  physStep,
  physForce
)
where

import           Asteroids.GameLogic.Asteroid
import           Asteroids.GameLogic.Physical
import           Asteroids.GameLogic.Ship
import           Asteroids.UILogic.Drawable
import           Pt2
import           Rand

data Entity = Entity
  { entity'Show :: String
  , entity'Draw :: IO ()
  , entity'Step :: Coord -> ShipState -> Entity
  }

entity :: (Drawable e, Show e) => (Coord -> ShipState -> Entity) -> e -> Entity
entity f e = Entity { entity'Show = show e
                    , entity'Draw = draw e
                    , entity'Step = f }

makeAsteroid :: Coord -> Int -> RandomState Entity
makeAsteroid size seed = do
  a <- createAsteroid size seed
  return $ entity (stepEntity asteroidStep a) a

makeShip :: Pt2 Coord -> Coord -> Entity
makeShip pos heading = let
    s = createShip pos heading
    in entity (stepEntity shipStep s) s

instance Show Entity
  where show e = entity'Show e

instance Drawable Entity
  where draw = entity'Draw

stepEntity :: (Drawable t, Show t) => (t -> Coord -> ShipState -> t) -> t -> Coord -> ShipState -> Entity
stepEntity f old c st = let
  new = f old c st
  in Entity { entity'Show = show new
            , entity'Draw = draw new
            , entity'Step = stepEntity f new }
