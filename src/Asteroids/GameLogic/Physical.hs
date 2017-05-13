module Asteroids.GameLogic.Physical (
  physStep,
  physForce,
  getUnitHeading,
  Physical(..)
)
where

import           Data.Int
import           Data.List        (intersperse)
import           GHC.Float
import           Graphics.UI.GLUT
import           Pt2
import           Shapes

data Physical = Physical {
  phys'Position :: Pt2,
  phys'Velocity :: Pt2,
  phys'Heading  :: Double,
  phys'Spin     :: Double
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

physForce :: Physical -> (Pt2->Pt2) -> (Double->Double) -> Physical
physForce old accel turn = old { phys'Velocity = vel', phys'Spin = spin' }
  where
    vel' = accel $ phys'Velocity old
    spin' = turn $ phys'Spin old

getUnitHeading :: Physical -> Pt2
getUnitHeading p = Pt2 (dx,dy)
  where
    heading = phys'Heading p
    radians = -heading * pi / 180
    dx = sin radians
    dy = cos radians

spaceLimit :: Double
spaceLimit = 1.2

modSpace :: Double -> Double
modSpace x
  | x >= -spaceLimit && x <= spaceLimit = x
  | otherwise = x'
      where
        a = x + spaceLimit
        b = a `remf'` (2 * spaceLimit)
        x' = if b < 0 then b + spaceLimit else b - spaceLimit

modPt2 :: Pt2 -> Pt2
modPt2 (Pt2 (x,y)) = Pt2 (modSpace x,modSpace y)

physStep :: Double -> Physical -> Physical
physStep dt old = old {phys'Position = pos', phys'Heading = heading' }
  where
    pos' = modPt2 $ phys'Position old + mulPt2 (phys'Velocity old) dt
    heading' = mod2pi $ phys'Heading old + (phys'Spin old) * dt

instance Shape Physical
  where
    drawGL phys =
      let Pt2 (x,y) = phys'Position phys
          a = phys'Heading phys
      in do
        translate $ Vector3 (double2Float x) (double2Float y) 0
        rotate (double2Float a) $ Vector3 0 0 (1::GLfloat)

