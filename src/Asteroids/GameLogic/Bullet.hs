module Asteroids.GameLogic.Bullet
  ( Bullet, newBullet
  , bulletAge
  ) where

import           Asteroids.GameLogic.Constants
import           Asteroids.GameLogic.Physical
import           Asteroids.Helpers
import           Asteroids.UILogic.Drawable

data Bullet = Bullet
  { bulletPos :: Physical
  , lastPos   :: Position
  , bulletAge :: TimeDelta } deriving Eq

instance Show Bullet where
  show b = "--BULLET--\n" ++ show (bulletPos b)

instance Drawable Bullet where
  draw bullet = innerDrawing $ do
    draw bulletColor
    draw (bulletPos bullet)
    mapM_ drawPoly bulletShape

instance Physics Bullet where
  physical = bulletPos
  step = stepBullet
  boundary = worldBoundary (pt2 bulletSize bulletSize)

newBullet :: Physical -> Bullet
newBullet p = Bullet { bulletPos = newPhys pos' vel' angle' spin'
                     , lastPos = physPos p
                     , bulletAge = 0 }
  where unit = getUnitHeading p
        pos = unit `mulPt2` shipSize
        vel =  unit `mulPt2` bulletSpeed
        vel' = vel + physVel p
        pos' = pos + physPos p
        angle' = physAngle p
        spin' = 360
                
stepBullet :: TimeDelta -> Bullet -> Bullet
stepBullet dt bullet = bullet'
  where age = dt + bulletAge bullet
        p' = step dt $ bulletPos bullet
        bullet' = bullet { bulletPos = p' `withSolid` [tracer bullet']
                         , bulletAge = age
                         , lastPos = physPos $ bulletPos bullet }

tracer :: Bullet -> [Pt2 Coord]
tracer b = [pt2Zero, dp]
  where p0 = physPos (bulletPos b)
        p1 = lastPos b
        dp = p0 - p1

bulletShape :: [Poly2]
bulletShape =
  [ pt2ToPoly [Pt2 (-bulletSize,0), Pt2 (bulletSize,0)]
  , pt2ToPoly [Pt2 (0,-bulletSize), Pt2 (0,bulletSize)] ]
