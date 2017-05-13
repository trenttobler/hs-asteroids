module Shapes (
  Shape(..)
) where

class Shape x where
    drawGL :: x -> IO ()