module Asteroids.GameLogic.Physical (
  Physical(..), newPhys,
  physStep,
  physForce,
  getUnitHeading
)
where

import           Asteroids.Helpers
import           Asteroids.UILogic.Drawable
import           Pt2

data Physical = Physical {
  phys'Position :: Pt2 Coord,
  phys'Velocity :: Pt2 Coord,
  phys'Heading  :: Coord,
  phys'Spin     :: Coord
}

instance Show Physical
  where
    show = showLabels "\n" [("pos:     ", (show . phys'Position)),
                            ("vel:     ", (show . phys'Velocity)),
                            ("heading: ", (show . phys'Heading)),
                            ("spin:    ", (show . phys'Spin))]

instance Drawable Physical
  where draw p = adjustOrigin (phys'Position p) (phys'Heading p)

newPhys :: Coord -> Coord -> Coord -> Coord -> Coord -> Coord -> Physical
newPhys x y dx dy w dw = Physical (pt2 x y) (pt2 dx dy) w dw

physForce :: Physical -> (Pt2 Coord->Pt2 Coord) -> (Coord->Coord) -> Physical
physForce old accel torque = old { phys'Velocity = vel', phys'Spin = spin' }
  where
    vel' = accel $ phys'Velocity old
    spin' = torque $ phys'Spin old

getUnitHeading :: Physical -> Pt2 Coord
getUnitHeading p = pt2 dx dy
  where
    heading = phys'Heading p
    radians = -heading * pi / 180
    dx = sin radians
    dy = cos radians

spaceLimitX :: (Coord,Coord)
spaceLimitY :: (Coord,Coord)
spaceLimitX = (-1.2,1.2)
spaceLimitY = (-1.2,1.2)

modPt2 :: Pt2 Coord -> Pt2 Coord
modPt2 (Pt2 (x,y)) = pt2 x' y'
  where x' = (modularInterval spaceLimitX x)
        y' = (modularInterval spaceLimitY y)

physStep :: Coord -> Physical -> Physical
physStep dt old = old {phys'Position = pos', phys'Heading = heading' }
  where
    pos' = modPt2 $ phys'Position old + mulPt2 (phys'Velocity old) dt
    heading' = modularInterval (0,360) $ phys'Heading old + (phys'Spin old) * dt

