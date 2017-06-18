module PolyPt2
  ( PolyPt2
  , polyAreaPt2
  , polyMidPt2
  , polyAreaMidPt2
  , polyNormPt2
  , polyPt2Lines
  , pt2InsideWindingPoly
  , pt2InsideAlternatingPoly

  , pt2PolyCrossingNumber, polyPt2Intersections
  , polyPt2WindingIntersections, polyPt2AlternatingIntersections

  , module LinePt2
  , module TriPt2
  ) where

import           LinePt2
import           TriPt2
import           Asteroids.Helpers

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
polyPt2Lines ps = LinePt2 <$> wrappedPairs ps

pt2PolyCrossingNumber :: (Eq a, Ord a, Num a) => Pt2 a -> [Pt2 a] -> Int
pt2PolyCrossingNumber p ps = sum crossingNumbers
  where crossingNumbers = fmap (pt2LineRightCrossingNumber p . LinePt2) sides
        sides = wrappedPairs ps

pt2InsideWindingPoly :: (Eq a, Ord a, Num a) => Pt2 a -> [Pt2 a] -> Bool
pt2InsideWindingPoly p ps = pt2PolyCrossingNumber p ps > 0

pt2InsideAlternatingPoly :: (Eq a, Ord a, Num a) => Pt2 a -> [Pt2 a] -> Bool
pt2InsideAlternatingPoly p ps = odd n
  where n = pt2PolyCrossingNumber p ps

polyPt2WindingIntersections :: (Eq a, Ord a, Num a) =>
  [PolyPt2 a] -> [(PolyPt2 a, PolyPt2 a)]
polyPt2WindingIntersections = polyPt2Intersections pt2InsideWindingPoly

polyPt2AlternatingIntersections :: (Eq a, Ord a, Num a) =>
  [PolyPt2 a] -> [(PolyPt2 a, PolyPt2 a)]
polyPt2AlternatingIntersections = polyPt2Intersections pt2InsideAlternatingPoly

polyPt2Intersections :: (Eq a, Ord a, Num a) =>
  (Pt2 a -> [Pt2 a] -> Bool) -> [PolyPt2 a] -> [(PolyPt2 a, PolyPt2 a)]
polyPt2Intersections pinside ps = do
  p1 <- ps
  p2 <- ps
  [(p1, p2) | p1 < p2 && intersects' p1 p2]
  where intersects' [] _ = False
        intersects' _ [] = False
        intersects' p1 p2 = pinside (head p1) p2
                         || pinside (head p2) p1
                         || sidesCross p1 p2
        sidesCross p1 p2 = any (anyCrossings $ polyPt2Lines p2) (polyPt2Lines p1)
        anyCrossings lineList line = any (linesCrossed line) lineList
