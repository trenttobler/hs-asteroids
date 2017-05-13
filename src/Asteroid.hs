module Asteroid (
  Asteroid(..),
  createAsteroid
) where

import           Graphics.Rendering.OpenGL
import           PolyPt2
import           Pt2
import           Shapes
import           System.Random
import           Utils

data Asteroid = Asteroid {
  asteroid'Draw :: IO (),
  asteroid'Seed :: Int,
  asteroid'Size :: Double
}

randomDistRange :: (Double, Double)
randomDistRange = (0.5,1.0)

randomAngleBias :: Double
randomAngleBias = 6.0

createAsteroid :: RandomGen r => Double -> Int -> r -> Asteroid
createAsteroid size seed r = Asteroid {
    asteroid'Draw = drawAsteroid $ asteroidVertices r size,
    asteroid'Seed = seed,
    asteroid'Size = size
  }

instance Show Asteroid
  where show a = concat ["Asteroid#", show (asteroid'Seed a),
                         " (",        show (asteroid'Size a), ")" ]

instance Shape Asteroid where drawGL a = asteroid'Draw a

drawAsteroid :: IO b -> IO b
drawAsteroid verts = do
  colorGL 0.5 0.5 0.5
  renderPrimitive LineLoop verts

randomSeq :: (Random a, RandomGen g) => Int -> (a, a) -> g -> [a]
randomSeq n (a,b) r = take n $ randomRs (a,b) r

randomAngles :: (Floating a, RandomGen t, Random a) => Int -> (a, a) -> t -> [a]
randomAngles n w r = angles
  where
    aseq = scanl1 (+) $ randomSeq n w r
    radians = 2 * pi / last aseq
    angles = fmap (radians *) aseq

asteroidVertices :: RandomGen r => r -> Double -> IO ()
asteroidVertices seed size = mapGLPt2s $ asteroidPt2s seed size

asteroidPt2s :: RandomGen r => r -> Double -> [Pt2]
asteroidPt2s r0 size = polyNormPt2 size pts
  where
    (ptCnt, r1) = randomR (12,24) r0
    (r2,r3) = split r1
    dists = randomSeq ptCnt randomDistRange r2
    angles = randomAngles ptCnt (1.0,randomAngleBias) r3
    point (d,a) = Pt2 (d * cos a,d * sin a)
    pts = point <$> zip dists angles
