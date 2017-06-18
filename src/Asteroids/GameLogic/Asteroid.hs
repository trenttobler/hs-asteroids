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

data Asteroid = Asteroid
  { asteroidDraw      :: IO ()
  , asteroidSeed      :: Int
  , asteroidSize      :: Coord
  , asteroidPhys      :: Physical
  , asteroidMaxPtDist :: Coord
  , asteroidRand      :: RandomSeq }

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

createRandomAsteroid :: Int -> AsteroidMass -> RandomState Asteroid
createRandomAsteroid seed size = do
  d <- randR distR
  a <- randR angleR
  pos <- createPos (Pt2 $ fromPolar d a) size
  createRandomAsteroid' seed size pos

createRandomAsteroid' :: Int -> Coord -> Physical -> RandomState Asteroid
createRandomAsteroid' seed size pos = do
  pts <- randPts size
  randSeq <- get
  let poly = pt2ToPoly pts
      asteroid = Asteroid { asteroidDraw = drawPoly poly
                          , asteroidSeed = seed
                          , asteroidSize = size
                          , asteroidPhys = pos `withSolid` [pts]
                          , asteroidMaxPtDist = maxPt2Dist pts
                          , asteroidRand = randSeq }
  return asteroid

blowUpAsteroid :: Asteroid -> [Asteroid]
blowUpAsteroid a = let
  rseq = asteroidRand a
  normSz s = fmap (\x -> x / sum s) s
  seed = asteroidSeed a
  pos = physPos $ asteroidPhys a
  sz = asteroidSize a
  createRandomChild sz' = createPos pos sz' >>= createRandomAsteroid' seed sz'
  blowUp = do
    sz' <- randR (2, 6) >>= randRs (0.5, 1.5)
    mapM createRandomChild
         (fmap (sz *) (normSz sz'))
  in if sz > tinyAsteroid
      then evalState blowUp rseq
      else []

createPos :: Position -> Coord -> RandomState Physical
createPos p size = do
  dx' <- randR dxR
  dy' <- randR dyR
  s <- randR dAngleR
  let (dx, dy) = (dx' / ss, dy' / ss )
      ss = sqrt size
      ds = s / size
  return $ newPhys p (pt2 dx dy) 0 ds

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
