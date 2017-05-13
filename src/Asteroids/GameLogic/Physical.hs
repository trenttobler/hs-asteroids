module Asteroids.GameLogic.Physical (
  physStep,
  physForce,
  getUnitHeading,
  Physical(..)
)
where

import           Data.Int
import           Data.List        (intersperse)
import           Graphics.UI.GLUT
import           Pt2
import           Shapes
import           Utils

data Physical = Physical {
  phys'Position :: Pt2 Coord,
  phys'Velocity :: Pt2 Coord,
  phys'Heading  :: Coord,
  phys'Spin     :: Coord
}

instance Show Physical
  where
    show p =
      let parts = [("pos", (show . phys'Position)),
                   ("vel", (show . phys'Velocity)),
                   ("heading", (show . phys'Heading)),
                   ("spin", (show . phys'Spin))]
          showPart (label, attr) = label ++ ": " ++ attr p
       in concat $ intersperse " " $ fmap showPart parts

remf' :: RealFrac a => a -> a -> a
remf' x y = x - y * fromIntegral (truncate (x/y)::Int64)

mod2pi :: RealFrac a => a -> a
mod2pi x = remf' x 360

physForce :: Physical -> (Pt2 Coord->Pt2 Coord) -> (Coord->Coord) -> Physical
physForce old accel turn = old { phys'Velocity = vel', phys'Spin = spin' }
  where
    vel' = accel $ phys'Velocity old
    spin' = turn $ phys'Spin old

getUnitHeading :: Physical -> Pt2 Coord
getUnitHeading p = Pt2 (dx,dy)
  where
    heading = phys'Heading p
    radians = -heading * pi / 180
    dx = sin radians
    dy = cos radians

spaceLimit :: Coord
spaceLimit = 1.2

modSpace :: Coord -> Coord
modSpace x
  | x >= -spaceLimit && x <= spaceLimit = x
  | otherwise = x'
      where
        a = x + spaceLimit
        b = a `remf'` (2 * spaceLimit)
        x' = if b < 0 then b + spaceLimit else b - spaceLimit

modPt2 :: Pt2 Coord -> Pt2 Coord
modPt2 (Pt2 (x,y)) = Pt2 (modSpace x,modSpace y)

physStep :: Coord -> Physical -> Physical
physStep dt old = old {phys'Position = pos', phys'Heading = heading' }
  where
    pos' = modPt2 $ phys'Position old + mulPt2 (phys'Velocity old) dt
    heading' = mod2pi $ phys'Heading old + (phys'Spin old) * dt

instance Shape Physical
  where
    drawGL phys =
      let Pt2 (x,y) = phys'Position phys
          a = phys'Heading phys
          x' = coordToGL x
          y' = coordToGL y
      in do
        translate $ Vector3 x' y' 0
        rotate a $ Vector3 0 0 (1::GLfloat)

