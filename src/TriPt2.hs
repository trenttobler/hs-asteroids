module TriPt2 (
  TriPt2(..),
  triMidPt2,
  triAreaPt2,
  triAreaMidPt2
) where

import           Pt2

newtype TriPt2 a = TriPt2 (Pt2 a,Pt2 a,Pt2 a) deriving Show

triMidPt2 :: Fractional a => TriPt2 a -> Pt2 a
triMidPt2 (TriPt2 (a,b,c)) = ( a + b + c ) `divPt2` 3

-- | NOTE: clockwise area is positive, counterclockwise is negative
triAreaPt2 :: Fractional a => TriPt2 a -> a
triAreaPt2 (TriPt2 (a,b,c)) = (a - c) `crossPt2` (a - b) / 2

-- | NOTE: clockwise area is positive, counterclockwise is negative
triAreaMidPt2 :: Fractional a => TriPt2 a -> (a, Pt2 a)
triAreaMidPt2 t = (triAreaPt2 t, triMidPt2 t)
