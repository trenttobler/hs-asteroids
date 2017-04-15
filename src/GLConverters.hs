module GLConverters (
  mapGLPt2,
  mapGLPt2s,
  colorGL,
  remf',
  mod2pi
) where

import Graphics.Rendering.OpenGL
import GHC.Float
import Data.Int

import Shapes

mapGLPt2::Pt2 -> IO ()
mapGLPt2 (Pt2 (x,y)) = vertex $ Vertex2 (double2Float x) (double2Float y)

mapGLPt2s::[Pt2] -> IO ()
mapGLPt2s ps = sequence_ $ fmap mapGLPt2 ps

colorGL::GLfloat->GLfloat->GLfloat->IO ()
colorGL x y z = color $ Color3 x y z

remf' :: RealFrac a => a -> a -> a
remf' x y = x - y * fromIntegral (truncate (x/y)::Int64)

mod2pi :: RealFrac a => a -> a
mod2pi x = remf' x 360
