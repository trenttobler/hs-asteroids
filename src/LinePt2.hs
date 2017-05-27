module LinePt2
  ( LinePt2(..), linePt2, lineP1, lineP2, lineDP
  , lineP1X, lineP1Y, lineP2X, lineP2Y, lineToPt2s
  , linesCrossed, lineCrossPt2, lineBoundaryCrossed
  , crossedLinePairsBrute, crossedLinePairs

  , LineSide
  , pointLineSide, lineSide

  , module Pt2
  ) where

import           Data.List (foldl')
import           Pt2

newtype LinePt2 a = LinePt2 (Pt2 a, Pt2 a) deriving Eq

lineP1, lineP2 :: LinePt2 t -> Pt2 t
lineP1 (LinePt2 (p1,_)) = p1
lineP2 (LinePt2 (_,p2)) = p2

lineP1X, lineP1Y, lineP2X, lineP2Y :: LinePt2 t -> t
lineP1X a = pt2X $ lineP1 a
lineP1Y a = pt2Y $ lineP1 a
lineP2X a = pt2X $ lineP2 a
lineP2Y a = pt2Y $ lineP2 a

lineDP :: Num t => LinePt2 t -> Pt2 t
lineDP (LinePt2 (p1,p2)) = p1 - p2

linePt2 :: a -> a -> a -> a -> LinePt2 a
linePt2 x1 y1 x2 y2 = LinePt2 (pt2 x1 y1, pt2 x2 y2)

lineToPt2s :: LinePt2 a -> [Pt2 a]
lineToPt2s x = [lineP1 x, lineP2 x]

instance Show a => Show (LinePt2 a) where
  show (LinePt2 (a,b)) = show a ++ " to " ++ show b

instance Ord a => Ord (LinePt2 a) where
  (LinePt2 a) <  (LinePt2 b) = a < b
  (LinePt2 a) >  (LinePt2 b) = a < b
  (LinePt2 a) <= (LinePt2 b) = a < b
  (LinePt2 a) >= (LinePt2 b) = a < b
  compare (LinePt2 a) (LinePt2 b) = compare a b
  max     (LinePt2 a) (LinePt2 b) = LinePt2 (max a b)
  min     (LinePt2 a) (LinePt2 b) = LinePt2 (min a b)


data LineSide = LeftSide
              | CenterSide
              | RightSide
  deriving ( Show, Enum, Eq )

pointLineSide :: (Num a, Ord a) => Pt2 a -> LinePt2 a -> LineSide
pointLineSide p line
    | d > 0 = LeftSide
    | d < 0 = RightSide
    | otherwise = CenterSide
  where d = lineDP line `crossPt2` ( lineP1 line - p )

lineSide :: (Num a, Ord a) => LinePt2 a -> LinePt2 a -> LineSide
lineSide a b
    | b1 /= RightSide && b2 /= RightSide = LeftSide
    | b1 /= LeftSide  && b2 /= LeftSide  = RightSide
    | otherwise = CenterSide
  where b1 = pointLineSide (lineP1 b) a
        b2 = pointLineSide (lineP2 b) a

linesCrossed :: (Num a, Ord a) => LinePt2 a -> LinePt2 a -> Bool
linesCrossed a b = lineSide a b == CenterSide && lineSide b a == CenterSide

type LinePair a = (LinePt2 a, LinePt2 a)

crossedLinePairsBrute :: (Num a, Ord a) => [LinePt2 a] -> [LinePt2 a] -> [LinePair a]
crossedLinePairsBrute x y = concatMap eachCross x
  where eachCross x' = (,) x' <$> filter (linesCrossed x') y

crossedLinePairs :: (Num a, Ord a) => [LinePt2 a] -> [LinePt2 a] -> [LinePair a]
crossedLinePairs [] _ = []
crossedLinePairs _ [] = []
crossedLinePairs (x:xs) ys = xx' ++ ls' ++ rs'
  where (leftX, centerX, rightX) = lineBuckets x xs
        (leftY, centerY, rightY) = lineBuckets x ys
        x'xSide = flip lineSide x
        x'xCenter x' = x'xSide x' == CenterSide
        pairX x' = (x,x')

        xx' = map pairX $ filter x'xCenter centerY
        ls' = crossedLinePairs (leftX ++ centerX) (leftY ++ centerY)
        rs' = crossedLinePairs (rightX ++ centerX) (rightY ++ centerY)

lineBuckets :: (Num a, Ord a) => LinePt2 a -> [LinePt2 a] -> ([LinePt2 a],[LinePt2 a],[LinePt2 a])
lineBuckets x = foldl' intoBucket ([],[],[])
  where intoBucket (l,c,r) x' = case lineSide x x' of
          LeftSide   -> (x':l,  c,   r )
          CenterSide -> (  l, x':c,  r )
          RightSide  -> (  l,   c, x':r)

lineCrossPt2 :: Fractional a => LinePt2 a -> LinePt2 a -> Pt2 a
lineCrossPt2 a b = pt2 x y
  where da = lineP1 a - lineP2 a
        db = lineP1 b - lineP2 b
        Pt2 (x1,y1) = lineP1 a
        Pt2 (x2,y2) = lineP2 a
        Pt2 (x3,y3) = lineP1 b
        Pt2 (x4,y4) = lineP2 b
        aa = x1 * y2 - y1 * x2
        bb = x3 * y4 - y3 * x4
        den = da `crossPt2` db
        x = ( aa * ( x3 - x4 ) - (x1 - x2 ) * bb ) / den
        y = ( aa * ( y3 - y4 ) - (y1 - y2 ) * bb ) / den

lineBoundaryCrossed :: ( Num a, Ord a ) => LinePt2 a -> LinePt2 a -> Bool
lineBoundaryCrossed a b = insideX && insideY
    where insideX = overlaps ax1 ax2 bx1 bx2
          insideY = overlaps ay1 ay2 by1 by2
          (ax1, ay1)        = pt2Tuple $ lineP1 a
          (ax2, ay2)        = pt2Tuple $ lineP2 a
          (bx1, by1)        = pt2Tuple $ lineP1 b
          (bx2, by2)        = pt2Tuple $ lineP2 b
          overlaps m n p q  = m <= q && p <= n

