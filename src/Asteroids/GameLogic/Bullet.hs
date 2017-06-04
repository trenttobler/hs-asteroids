module Asteroids.GameLogic.Bullet
  ( Bullet, newBullet
  , bulletAge
  ) where

import           Asteroids.GameLogic.Constants
import           Asteroids.GameLogic.Physical
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
    let shape = pt2ToPoly [ physPos $ bulletPos bullet ]
    drawPoints shape

instance Physics Bullet where
  physical = bulletPos
  step = stepBullet
  boundary = worldBoundary (pt2 bulletSize bulletSize)

newBullet :: Physical -> Bullet
newBullet p = Bullet { bulletPos = newPhys pos' vel' angle' 0
                     , lastPos = physPos p
                     , bulletAge = 0 }
  where unit = getUnitHeading p
        pos = unit `mulPt2` shipSize
        vel =  unit `mulPt2` bulletSpeed
        vel' = vel + physVel p
        pos' = pos + physPos p
        angle' = physAngle p

stepBullet :: TimeDelta -> Bullet -> Bullet
stepBullet dt bullet = bullet'
  where age = dt + bulletAge bullet
        p' = step dt (bulletPos bullet)
        bullet' = bullet { bulletPos = p' `withSolid` [tracer bullet']
                         , bulletAge = age
                         , lastPos = physPos $ bulletPos bullet }

tracer :: Bullet -> [Pt2 Coord]
tracer b = [pt2Zero, dp]
  where p0 = physPos (bulletPos b)
        p1 = lastPos b
        dp = p0 - p1