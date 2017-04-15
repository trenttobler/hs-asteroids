module Asteroid (
  Asteroid,
  asteroidSeed,
  asteroidSize,
  asteroid
) where

import Data.List
import System.Random
import Graphics.Rendering.OpenGL
import Control.Applicative

import Rand
import Shapes
import GLConverters

data Asteroid = Asteroid (IO ()) Int Double
{-{
    !drawing::IO (),
    asteroidSeed::Int,
    asteroidSize::Double
}-}

asteroidSeed (Asteroid _ s _ ) = s
asteroidSize (Asteroid _ _ sz) = sz

asteroid seed size = Asteroid draw seed size
  where
    draw = drawAsteroid verts
    verts = asteroidVertices seed size

instance Show Asteroid
  where
    show (Asteroid _ seed size) = "Asteroid " ++ show seed ++ " " ++ show size

instance Shape Asteroid
  where
    drawGL (Asteroid d _ _ ) = d

drawAsteroid verts = do
  colorGL 0.5 0.5 0.5
  renderPrimitive LineLoop verts

randomSeq n (min,max) r = take n $ randomRs (min,max) r

randomAngles n w r = angles
  where
    aseq = scanl1 (+) $ randomSeq n w r
    radians = 2 * pi / last aseq
    angles = fmap (radians *) aseq

asteroidVertices seed size = mapGLPt2s $ asteroidPt2s seed size

asteroidPt2s seed size = polyNormPt2 size pts
  where
    r0 = seededRandomSeq seed
    (ptCnt, r1) = randomR (12,24) r0
    (r2,r3) = split r1
    dists = randomSeq ptCnt (0.4,1.0) r2
    angles = randomAngles ptCnt (1.0,4.0) r3
    toPt2 (d,a) = Pt2 (d * cos a,d * sin a)
    pts = toPt2 <$> zip dists angles
