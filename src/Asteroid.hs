module Asteroid (
  Asteroid(..),
  createAsteroid
) where

import           Graphics.Rendering.OpenGL
import           PolyPt2
import           Pt2
import           Shapes
import           Rand
import           Utils
import           Asteroids.GameLogic.Physical

data Asteroid = Asteroid {
  asteroid'Draw :: IO (),
  asteroid'Seed :: Int,
  asteroid'Size :: Coord
}

randomDistRange :: (Coord, Coord)
randomDistRange = (0.5,1.0)

randomAngleBias :: Coord
randomAngleBias = 6.0

createAsteroid :: Coord -> Int -> RandomState (Asteroid, Physical)
createAsteroid size seed = do
  pts <- asteroidPt2s size
  dist  <- randR (  0.4  ,  1.0    )
  angle <- randR (  0    ,  2*pi   )
  dx    <- randR ( -0.03,  0.03    )
  dy    <- randR ( -0.03,  0.03    )
  spin  <- randR ( -1,     1       )
  let pos = Pt2 (dist * cos angle, dist * sin angle)
      vel = Pt2 (dx,dy) `divPt2` sqrt size
      spin' = spin / size
      asteroid = Asteroid { asteroid'Draw = drawAsteroid $ asteroidVertices pts,
                            asteroid'Seed = seed,
                            asteroid'Size = size }
      phys = Physical { phys'Position = pos
                      , phys'Velocity = vel
                      , phys'Heading = 0
                      , phys'Spin = spin' }
  return (asteroid, phys)

instance Show Asteroid
  where show a = concat ["Asteroid#", show (asteroid'Seed a),
                         " (",        show (asteroid'Size a), ")" ]

instance Shape Asteroid where drawGL a = asteroid'Draw a

drawAsteroid :: IO b -> IO b
drawAsteroid verts = do
  colorGL 0.5 0.5 0.5
  renderPrimitive LineLoop verts

randomAngles :: (Random a, RealFloat a) => Int -> (a, a) -> RandomState [a]
randomAngles n w = do
  aseq' <- randRs w n
  let aseq = scanl1 (+) aseq'
      radians = 2 * pi / last aseq
      angles = fmap (radians *) aseq
  return angles

asteroidVertices :: [Pt2 Coord] -> IO ()
asteroidVertices pts = mapGLPt2s $ pts

asteroidPt2s :: Coord -> RandomState [Pt2 Coord]
asteroidPt2s size = do
  ptCnt <- randR (12,24)
  dists <- randRs randomDistRange ptCnt
  angles <- randomAngles ptCnt (1.0,randomAngleBias)
  let pts = point <$> zip dists angles
      point (d,a) = Pt2 (d * cos a,d * sin a)
  return $ polyNormPt2 size pts
