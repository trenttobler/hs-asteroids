module Asteroids.GameLogic.Physical
  ( Physics(..)
  , Physical(..)
  , TimeDelta   
  , Position    
  , Velocity    
  , Acceleration
  , Angle       
  , Spin        
  , Torque      
  , newPhys
  , physForce
  , getUnitHeading
  ) where

import           Asteroids.Helpers
import           Asteroids.UILogic.Drawable

type TimeDelta    = Coord
type Position     = Pt2 Coord
type Velocity     = Pt2 Coord
type Acceleration = Velocity -> Velocity
type Angle        = Coord
type Spin         = Coord
type Torque       = Spin -> Spin

class Physics a where
  physical :: a -> Physical
  step :: TimeDelta -> a -> a

data Physical = Physical
  { physPos   :: Position
  , physVel   :: Velocity
  , physAngle :: Angle
  , physSpin  :: Spin     }

instance Show Physical where
  show = showLabels "\n" [("pos:     ", show . physPos),
                          ("vel:     ", show . physVel),
                          ("heading: ", show . physAngle),
                          ("spin:    ", show . physSpin)]

instance Drawable Physical
  where draw p = adjustOrigin (physPos p) (physAngle p)

instance Physics Physical where
  physical a = a
  step = physStep

newPhys :: Coord -> Coord -> Coord -> Coord -> Coord -> Coord -> Physical
newPhys x y dx dy = Physical (pt2 x y) (pt2 dx dy)

physForce :: Acceleration -> Torque -> Physical -> Physical
physForce accel torque old = old { physVel = vel', physSpin = spin' }
  where vel' = accel $ physVel old
        spin' = torque $ physSpin old

getUnitHeading :: Physical -> Pt2 Coord
getUnitHeading p = pt2 dx dy
  where a = physAngle p
        r = -a * pi / 180
        (dx,dy) = (sin r, cos r)

spaceLimitX :: (Coord,Coord)
spaceLimitY :: (Coord,Coord)
spaceLimitX = (-1.2,1.2)
spaceLimitY = (-1.2,1.2)

modPt2 :: Position -> Position
modPt2 (Pt2 (x,y)) = pt2 x' y'
  where x' = modularInterval spaceLimitX x
        y' = modularInterval spaceLimitY y

physStep :: TimeDelta -> Physical -> Physical
physStep dt old = old {physPos = pos', physAngle = heading' }
  where pos' = modPt2 $ physPos old + mulPt2 (physVel old) dt
        heading' = modularInterval (0,360) $ physAngle old + physSpin old * dt

