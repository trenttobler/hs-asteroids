module Asteroids.GameLogic.Asteroid
  ( module Asteroids.GameLogic.Physical
  , Asteroid
  , createRandomAsteroid
  , blowUpAsteroid
  ) where

import           Asteroids.GameLogic.Constants
import           Asteroids.GameLogic.Physical
import           Asteroids.Helpers
import           Asteroids.UILogic.Drawable
import           Rand

data Asteroid = Asteroid {
  asteroidDraw      :: IO (),
  asteroidSeed      :: Int,
  asteroidSize      :: Coord,
  asteroidPhys      :: Physical,
  asteroidMaxPtDist :: Coord
}

instance Show Asteroid
  where show a = concat ["Asteroid#", show (asteroidSeed a),
                         " (",        show (asteroidSize a), ")" ]

instance Drawable Asteroid where
  draw a = innerDrawing $ do
    draw $ asteroidPhys a
    draw asteroidColor
    asteroidDraw a

instance Physics Asteroid where
  physical = asteroidPhys
  step dt a = a { asteroidPhys = step dt (asteroidPhys a) }
  boundary a = worldBoundary (ptDist a) a
    where ptDist = (\d -> pt2 d d) . asteroidMaxPtDist

type AsteroidMass = Coord

randomDistRange :: (Coord, Coord)
randomDistRange = (0.5,1.0)

randomAngleBias :: Coord
randomAngleBias = 6.0

createRandomAsteroid :: AsteroidMass -> Int -> RandomState Asteroid
createRandomAsteroid size seed = do
  pts <- randPts size
  pos <- createPos size
  let asteroid = Asteroid { asteroidDraw = drawPoly poly
                          , asteroidSeed = seed
                          , asteroidSize = size
                          , asteroidPhys = pos `withSolid` polyPt2Lines pts
                          , asteroidMaxPtDist = maxPt2Dist pts }
      poly = pt2ToPoly pts
  return asteroid

blowUpAsteroid :: Asteroid -> [Asteroid]
blowUpAsteroid _todoAsteroid = []

createPos :: Coord -> RandomState Physical
createPos size = do
  d <- randR distR
  a <- randR angleR
  dx' <- randR dxR
  dy' <- randR dyR
  s <- randR dAngleR
  let (x, y) = fromPolar d a
      (dx, dy) = fmap (/ sqrt size) (dx', dy')
      ds = s / size
  return $ newPhys (pt2 x y) (pt2 dx dy) 0 ds

distR :: (Coord,Coord)
angleR :: (Coord,Coord)
dxR :: (Coord,Coord)
dyR :: (Coord,Coord)
dAngleR :: (Coord,Coord)
distR = (0.4,1.0)
angleR = (0,2*pi)
dxR = (-0.03,0.03)
dyR = (-0.03,0.03)
dAngleR = (-1,1)

randomAngles :: (Random a, RealFloat a) => Int -> (a, a) -> RandomState [a]
randomAngles n w = do
  aseq' <- randRs w n
  let aseq = scanl1 (+) aseq'
      radians = 2 * pi / last aseq
      angles = fmap (radians *) aseq
  return angles

randPts :: Coord -> RandomState [Pt2 Coord]
randPts size = do
  ptCnt <- randR (12,24)
  dists <- randRs randomDistRange ptCnt
  angles <- randomAngles ptCnt (1.0,randomAngleBias)
  let pts = pt2PolarRadians <$> zip dists angles
      pts' = polyNormPt2 size pts
  return pts'
