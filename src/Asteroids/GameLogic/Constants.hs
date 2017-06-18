module Asteroids.GameLogic.Constants
  ( module Asteroids.GameLogic.Constants
  ) where

import           Asteroids.GameLogic.Physical
import           Asteroids.UILogic.Drawable

asteroidColor, shipColor, bulletColor, thrustColor, explosionColor :: PixelColor
lifeColor, scoreColor :: PixelColor

asteroidColor = pixelColor 0.5 0.5 0.5
shipColor = pixelColor 1 1 1
bulletColor = pixelColor 0.9 1 1
thrustColor = pixelColor 1 0 0
explosionColor = pixelColor 1 0.5 0.2
lifeColor = pixelColor 0.5 0 1
scoreColor = pixelColor 0.9 1 1

explosionSize :: Coord
explosionSpeed :: Coord
burstAngle :: Coord
burstCount :: Int
explosionSize = 0.25
burstAngle = 2.0 * pi / fromIntegral burstCount
explosionSpeed = 1
burstCount = 7

lifeSpacing :: Pt2 Coord
lifePos :: Pt2 Coord
lifeSize :: Coord
lifePos = Pt2 (-0.95, 0.95)
lifeSpacing = Pt2 (0.05,0)
lifeSize = 0.015

scorePos :: Pt2 Coord
scorePos = Pt2 (0.95,0.95)

shipSize :: Coord
shipTurnRate :: Coord
shipSize = 0.04
shipTurnRate = 360

bulletSize :: Coord
bulletSize = 0.005

bulletSpeed :: Coord
bulletRange :: Coord
maxBulletAge :: TimeDelta
bulletSpeed = 0.5
bulletRange = 1.4
maxBulletAge = bulletRange / bulletSpeed

startingAsteroidCount :: Int
startingAsteroidCount = 3

largeAsteroid :: Coord
tinyAsteroid :: Coord
largeAsteroid = 0.10
tinyAsteroid = 0.01