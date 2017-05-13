module Utils (
  mapGLPt2,
  mapGLPt2s,
  colorGL,
) where

import           GHC.Float
import           Graphics.Rendering.OpenGL
import           Pt2

mapGLPt2::Pt2 -> IO ()
mapGLPt2 (Pt2 (x,y)) = vertex $ Vertex2 (double2Float x) (double2Float y)

mapGLPt2s::[Pt2] -> IO ()
mapGLPt2s ps = sequence_ $ fmap mapGLPt2 ps

colorGL::GLfloat->GLfloat->GLfloat->IO ()
colorGL x y z = color $ Color3 x y z
