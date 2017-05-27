module PolyPt2
  ( PolyPt2
  , polyAreaPt2
  , polyMidPt2
  , polyAreaMidPt2
  , polyNormPt2
  , polyPt2Lines

  , module LinePt2
  , module TriPt2
  ) where

import           LinePt2
import           TriPt2

type PolyPt2 a = [Pt2 a]

polyToTriPt2 :: [Pt2 a] -> [TriPt2 a]
polyToTriPt2 (a:b:c:ps) = TriPt2 (a,b,c) : polyToTriPt2 (a:c:ps)
polyToTriPt2 _          = []

-- | NOTE: clockwise area is positive, counterclockwise is negative
polyAreaPt2 :: Fractional a => [Pt2 a] -> a
polyAreaPt2 p = sum $ fmap triAreaPt2 (polyToTriPt2 p )

-- | NOTE: clockwise area is positive, counterclockwise is negative
polyAreaMidPt2 :: Fractional a => [Pt2 a] -> (a, Pt2 a)
polyAreaMidPt2 p = (totalArea, centroid)
  where
    tris = polyToTriPt2 p
    tps = fmap (triPart . triAreaMidPt2) tris
    triPart (area,mid) = (area,mulPt2 mid area)
    totalArea = sum $ fmap fst tps
    centroid = divPt2 (sum $ fmap snd tps) totalArea

polyMidPt2 :: Fractional a => [Pt2 a] -> Pt2 a
polyMidPt2 ps = snd $ polyAreaMidPt2 ps

-- | Normalizes the polygon to have area of sz.
polyNormPt2 :: (Floating a, Ord a) => a -> [Pt2 a] -> [Pt2 a]
polyNormPt2 sz pts = fmap norm pts'
  where
    (a, midP) = polyAreaMidPt2 pts
    da = sqrt ( abs ( sz /  a ) )
    norm p = mulPt2 ( p - midP ) da
    pts' = if signum a * signum sz < 0 then reverse pts else pts

polyPt2Lines :: [Pt2 a] -> [LinePt2 a]
polyPt2Lines [] = []
polyPt2Lines ps = wrappedPairs (head ps) ps []

wrappedPairs :: Pt2 a -> [Pt2 a] -> [LinePt2 a] -> [LinePt2 a]
wrappedPairs _ [] _ = []
wrappedPairs a [z] pqs = LinePt2 (z, a) : pqs
wrappedPairs a (b:c:zs) pqs = wrappedPairs a (c:zs) (LinePt2 (b, c) : pqs)
