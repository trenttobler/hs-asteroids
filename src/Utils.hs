module Utils (
  mapGLPt2,
  mapGLPt2s,
  colorGL,
  coordToGL,
  Coord
) where

import           Graphics.Rendering.OpenGL (vertex
                                           , Vertex2(..), GLfloat
                                           , color , Color3(..))
import           Pt2

type Coord = GLfloat

coordToGL:: Coord -> GLfloat
coordToGL = id

mapGLPt2::Pt2 Coord -> IO ()
mapGLPt2 (Pt2 (x,y)) = vertex $ Vertex2 (coordToGL x) (coordToGL y)

mapGLPt2s::[Pt2 Coord] -> IO ()
mapGLPt2s ps = sequence_ $ fmap mapGLPt2 ps

colorGL::GLfloat->GLfloat->GLfloat->IO ()
colorGL x y z = color $ Color3 x y z
