module Asteroids.GameLogic.Asteroid (
  Asteroid,
  createAsteroid,
  asteroidStep
) where

import           Asteroids.GameLogic.Physical
import           Asteroids.UILogic.Drawable
import           PolyPt2
import           Pt2
import           Rand

data Asteroid = Asteroid {
  asteroid'Draw :: Asteroid -> IO (),
  asteroid'Seed :: Int,
  asteroid'Size :: Coord,
  asteroid'Phys :: Physical
}

instance Show Asteroid
  where show a = concat ["Asteroid#", show (asteroid'Seed a),
                         " (",        show (asteroid'Size a), ")" ]

instance Drawable Asteroid where
  draw a = (asteroid'Draw a) a

randomDistRange :: (Coord, Coord)
randomDistRange = (0.5,1.0)

randomAngleBias :: Coord
randomAngleBias = 6.0

createAsteroid :: Coord -> Int -> RandomState Asteroid
createAsteroid size seed = do
  pts <- randPts size
  pos <- createPos size
  let asteroid = Asteroid { asteroid'Draw = drawAsteroid poly,
                            asteroid'Seed = seed,
                            asteroid'Size = size,
                            asteroid'Phys = pos }
      poly = pt2ToPoly pts
  return asteroid

asteroidStep :: Asteroid -> Coord -> a -> Asteroid
asteroidStep old dt _ = old { asteroid'Phys = physStep dt (asteroid'Phys old) }

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
  return $ newPhys x y dx dy 0 ds

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

fromPolar :: Floating t => t -> t -> (t, t)
fromPolar d a = (d * cos a, d * sin a)

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
  let pts = point <$> zip dists angles
      point (d,a) = Pt2 (d * cos a,d * sin a)
      pts' = polyNormPt2 size pts
  return pts'

inAsteroidColor :: IO ()
inAsteroidColor = drawColor 0.5 0.5 0.5

drawAsteroid :: Poly2 -> Asteroid -> IO ()
drawAsteroid poly a = innerDrawing $ do
  draw $ asteroid'Phys a
  inAsteroidColor
  drawPoly poly
