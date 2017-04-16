module Shapes (
  Shape(..),
  Pt2(..),
  dotPt2,
  crossPt2,
  mulPt2,
  divPt2,
  toPt2,
  midPt2,
  TriPt2(..),
  triMidPt2,
  triAreaPt2,
  triAreaMidPt2,
  polyAreaPt2,
  polyMidPt2,
  polyAreaMidPt2,
  polyNormPt2
) where

class Shape x where
    drawGL :: x -> IO ()

newtype Pt2 = Pt2 (Double,Double) deriving Eq

instance Show Pt2 where
  show (Pt2 (x,y)) = "<Pt2>(" ++ show x ++ ", " ++ show y ++ ")"

instance Num Pt2 where
  Pt2 (x1,y1) + Pt2 (x2,y2) = Pt2 (x1+x2, y1+y2)
  Pt2 (x1,y1) - Pt2 (x2,y2) = Pt2 (x1-x2, y1-y2)
  Pt2 (x1,y1) * Pt2 (x2,y2) = Pt2 (x1*x2, y1*y2)
  abs (Pt2 (x,y))           = Pt2 (abs x, abs y)
  signum (Pt2 (x,y))        = Pt2 (signum x,signum y)
  fromInteger i             = Pt2 (fromInteger i,fromInteger i)

dotPt2 :: Pt2 -> Pt2 -> Double
dotPt2   (Pt2 (x1,y1)) (Pt2 (x2,y2)) = x1 * x2 + y1 * y2

crossPt2 :: Pt2 -> Pt2 -> Double
crossPt2 (Pt2 (x1,y1)) (Pt2 (x2,y2)) = x1 * y2 - y1 * x2

mulPt2 :: Pt2 -> Double -> Pt2
mulPt2 (Pt2 (x,y)) s  = Pt2 (x*s, y*s)

divPt2 :: Pt2 -> Double -> Pt2
divPt2 (Pt2 (x,y)) s  = Pt2 (x/s, y/s)

toPt2 :: Double -> Pt2
toPt2     s    = Pt2 (s,s)

midPt2 :: Pt2 -> Pt2 -> Pt2
midPt2    a b  = divPt2 (a + b) 2

newtype TriPt2 = TriPt2 (Pt2,Pt2,Pt2) deriving Show

triMidPt2 :: TriPt2 -> Pt2
triMidPt2 (TriPt2 (a,b,c)) = divPt2 ( a + b + c ) 3

-- | NOTE: clockwise area is positive, counterclockwise is negative
triAreaPt2 :: TriPt2 -> Double
triAreaPt2 (TriPt2 (a,b,c)) = crossPt2 ac ab / 2
  where
    ab = a - b
    ac = a - c

-- | NOTE: clockwise area is positive, counterclockwise is negative
triAreaMidPt2 :: TriPt2 -> (Double, Pt2)
triAreaMidPt2 t = (area, mid)
  where
    mid = triMidPt2 t
    area = triAreaPt2 t

polyToTriPt2 :: [Pt2] -> [TriPt2]
polyToTriPt2 (a:b:c:ps) = TriPt2 (a,b,c) : polyToTriPt2 (a:c:ps)
polyToTriPt2 _          = []

-- | NOTE: clockwise area is positive, counterclockwise is negative
polyAreaPt2 :: [Pt2] -> Double
polyAreaPt2 p = sum $ fmap triAreaPt2 (polyToTriPt2 p )

-- | NOTE: clockwise area is positive, counterclockwise is negative
polyAreaMidPt2 :: [Pt2] -> (Double, Pt2)
polyAreaMidPt2 p = (totalArea, centroid)
  where
    tris = polyToTriPt2 p
    tps = fmap (triPart . triAreaMidPt2) tris
    triPart (area,mid) = (area,mulPt2 mid area)
    totalArea = sum $ fmap fst tps
    centroid = divPt2 (sum $ fmap snd tps) totalArea

polyMidPt2 :: [Pt2] -> Pt2
polyMidPt2 ps = snd $ polyAreaMidPt2 ps

-- | Normalizes the polygon to have area of sz.
polyNormPt2 :: Double -> [Pt2] -> [Pt2]
polyNormPt2 sz pts = fmap norm pts'
  where
    (a, midP) = polyAreaMidPt2 pts
    da = sqrt ( abs ( sz /  a ) )
    norm p = mulPt2 ( p - midP ) da
    pts' = if signum a * signum sz < 0 then reverse pts else pts
