module Entity (
  makeAsteroid,
  makeShip,
  physStep,
  physForce,
  Entity(..),
  entityStep
)
where

import           Asteroid
import           Data.Int
import           GHC.Float
import           GLConverters
import           Graphics.UI.GLUT
import           Rand
import           Shapes
import           System.Random

newtype Physical = Physical (Pt2,Pt2,Double,Double)

instance Show Physical
  where
    show (Physical (pos,vel,a,da)) =
      "pos:" ++ show pos ++ " vel:" ++ show vel ++ " heading:" ++ show a ++ " spin:" ++ show da

remf' :: RealFrac a => a -> a -> a
remf' x y = x - y * fromIntegral (truncate (x/y)::Int64)

mod2pi :: RealFrac a => a -> a
mod2pi x = remf' x 360

physForce :: Physical -> (Pt2->Pt2) -> (Double->Double) -> Physical
physForce old accel turn = Physical (pos, vel', heading, spin')
  where
    Physical (pos, vel, heading, spin) = old
    vel' = accel vel
    spin' = turn spin

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
physStep dt old = Physical (pos', vel, heading', spin )
  where
    Physical (pos, vel, heading, spin) = old
    pos' = modPt2 $ pos + mulPt2 vel dt
    heading' = mod2pi $ heading + spin * dt

instance Shape Physical
  where
    drawGL phys =
      let
        Physical (Pt2 (x,y),_,a,_) = phys
      in do
        translate $ Vector3 (double2Float x) (double2Float y) 0
        rotate (double2Float a) $ Vector3 0 0 (1::GLfloat)

data Entity = PhysicalAsteroid (Asteroid,Physical)
            | PhysicalShip Physical

makeAsteroid :: Double -> Int -> Entity
makeAsteroid size seed = PhysicalAsteroid (asteroid size seed r', Physical (pos,vel,0,spin'))
  where
    r = seededRandomSeq seed
    (dist,  r1) = randomR (  0.4  ,  1.0    ) r
    (angle, r2) = randomR (  0    ,  2*pi   ) r1
    (dx,    r3) = randomR ( -0.03,  0.03    ) r2
    (dy,    r4) = randomR ( -0.03,  0.03    ) r3
    (spin,  r5) = randomR ( -3    ,  3      ) r4
    r' = r5
    pos = Pt2 (dist * cos angle, dist * sin angle)
    vel =  Pt2 (dx,dy) `divPt2` sqrt size
    spin' = spin / size

makeShip :: Pt2 -> Double -> Entity
makeShip pos heading = PhysicalShip $ Physical (pos, Pt2 (0,0), heading, 0)

instance Show Entity
  where show (PhysicalAsteroid (a,p)) = show a ++ " " ++ show p
        show (PhysicalShip p)         = "Ship at " ++ show p

instance Shape Entity
  where
    drawGL (PhysicalAsteroid (a,p)) = preservingMatrix $ do
      drawGL p
      drawGL a
    drawGL (PhysicalShip p) = preservingMatrix $ do
      drawGL p
      renderPrimitive LineLoop shipShape


-- At some point, will want to break ship out into it's own class.
-- That will also require moving out the physical type to prevent
-- an import loop.
shipSize :: Double
shipSize = 0.02

shipPts :: [Pt2]
shipPts = [Pt2 (-shipSize,-shipSize), Pt2(0,2*shipSize), Pt2(shipSize,-shipSize)]

shipShape :: IO ()
shipShape = mapGLPt2s shipPts

entityStep :: Double -> Entity -> Entity
entityStep dt (PhysicalAsteroid (a,p)) = PhysicalAsteroid (a, physStep dt p )
entityStep dt (PhysicalShip p)         = PhysicalShip $ physStep dt p
