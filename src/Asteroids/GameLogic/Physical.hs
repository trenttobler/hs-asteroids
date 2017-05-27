module Asteroids.GameLogic.Physical
  ( Physics(..)
  , Physical, physPos, physVel, physAngle, physSpin, solidLines
  , TimeDelta
  , Position
  , Velocity
  , Acceleration
  , Angle
  , Spin
  , Torque
  , SolidLine
  , newPhys
  , physForce
  , withSolid
  , solidWorldLines
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
type SolidLine    = LinePt2 Coord

class Physics a where
  physical :: a -> Physical
  step :: TimeDelta -> a -> a

data Physical = Physical
  { physPos    :: Position
  , physVel    :: Velocity
  , physAngle  :: Angle
  , physSpin   :: Spin
  , solidLines :: [SolidLine]     }

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

newPhys :: Pt2 Coord -> Pt2 Coord -> Coord -> Coord -> Physical
newPhys pos vel angle spin
  = Physical { physPos = pos
             , physVel = vel
             , physAngle = angle
             , physSpin = spin
             , solidLines = [] }

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


withSolid :: Physical -> [SolidLine] -> Physical
withSolid p s = p { solidLines = s }

solidWorldLines :: Physical -> [SolidLine]
solidWorldLines p = fmap toWorldLine (solidLines p)
  where toWorldLine line = LinePt2 ( toWorldPoint $ lineP1 line
                                   , toWorldPoint $ lineP2 line )
        toWorldPoint = translate' . rotate'
        a = physAngle p
        r = -a * pi / 180
        (u', v') = (cos r, sin r)
        (dx, dy) = pt2Tuple (physPos p)
        rotate' pt = pt2 (u' * pt2X pt + v' * pt2Y pt)
                         (u' * pt2Y pt - v' * pt2X pt)
        translate' pt = pt + Pt2 (dx, dy)
