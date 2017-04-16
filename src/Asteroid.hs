module Asteroid (
  Asteroid,
  asteroidSeed,
  asteroidSize,
  asteroid
) where

import           Graphics.Rendering.OpenGL
import           System.Random
import           GLConverters
import           Shapes

newtype Asteroid = Asteroid (IO (), Int, Double)

asteroidSeed :: Asteroid -> Int
asteroidSeed (Asteroid (_, s, _ )) = s

asteroidSize :: Asteroid -> Double
asteroidSize (Asteroid (_, _, sz)) = sz

asteroid :: RandomGen r => Double -> Int -> r -> Asteroid
asteroid size seed r = Asteroid (draw, seed, size)
  where
    draw = drawAsteroid verts
    verts = asteroidVertices r size

instance Show Asteroid
  where
    show (Asteroid (_, seed, size)) = "Asteroid#" ++ show seed ++ " (" ++ show size ++ ")"

instance Shape Asteroid
  where
    drawGL (Asteroid (d, _, _) ) = d

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
    dists = randomSeq ptCnt (0.4,1.0) r2
    angles = randomAngles ptCnt (1.0,4.0) r3
    point (d,a) = Pt2 (d * cos a,d * sin a)
    pts = point <$> zip dists angles
