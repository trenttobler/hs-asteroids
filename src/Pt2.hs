module Pt2
  ( Pt2(..), pt2, pt2X, pt2Y, pt2Tuple, pt2Zero
  , dotPt2, crossPt2
  , mulPt2, divPt2, midPt2
  , maxPt2Dist
  , pt2PolarRadians, pt2PolarDegrees
  ) where

import           Data.List                    (foldl')

newtype Pt2 a = Pt2 (a,a) deriving Eq

pt2 :: a -> a -> Pt2 a
pt2 x y = Pt2 (x,y)

pt2Tuple :: Pt2 t -> (t, t)
pt2Tuple (Pt2 c)  = c

pt2X, pt2Y :: Pt2 t -> t
pt2X (Pt2 (x,_)) = x
pt2Y (Pt2 (_,y)) = y

pt2Zero :: Num a => Pt2 a
pt2Zero = pt2 0 0

pt2PolarRadians :: Floating a => (a,a) -> Pt2 a
pt2PolarRadians (d, a) = Pt2 (d * cos a,d * sin a)

pt2PolarDegrees :: Floating a => (a,a) -> Pt2 a
pt2PolarDegrees (d, a) = Pt2 (d * cos a',d * sin a')
  where a' = a * pi / 180

instance Show a => Show (Pt2 a) where show p = show (x,y) where Pt2 (x,y) = p

instance Num a => Num (Pt2 a) where
  (+)             = _xyOp (+)
  (-)             = _xyOp (-)
  (*)             = _xyOp (*)
  abs             = _unaryOp abs
  signum          = _unaryOp signum
  fromInteger i   = Pt2 (fromInteger i,fromInteger i)

instance Ord a => Ord (Pt2 a) where
  (Pt2 a) <  (Pt2 b) = a < b
  (Pt2 a) >  (Pt2 b) = a < b
  (Pt2 a) <= (Pt2 b) = a < b
  (Pt2 a) >= (Pt2 b) = a < b
  compare (Pt2 a) (Pt2 b) = compare a b
  max     (Pt2 a) (Pt2 b) = Pt2 (max a b)
  min     (Pt2 a) (Pt2 b) = Pt2 (min a b)


instance Functor Pt2 where
  fmap f (Pt2 (a,b)) = Pt2 (f a, f b)

mulPt2 :: Num a => Pt2 a -> a  -> Pt2 a
mulPt2            = _scalarOp (*)

divPt2 :: Fractional a => Pt2 a -> a -> Pt2 a
divPt2            = _scalarOp (/)

dotPt2 ::Num a => Pt2 a -> Pt2 a -> a
dotPt2 p1 p2 = xx + yy  where Pt2 (xx,yy) = p1 * p2

crossPt2 :: Num a => Pt2 a -> Pt2 a -> a
crossPt2 p1 p2 = x1 * y2 - y1 * x2
  where Pt2 (x1,y1) = p1
        Pt2 (x2,y2) = p2

midPt2 :: Fractional a => Pt2 a -> Pt2 a -> Pt2 a
midPt2    a b  = divPt2 (a + b) 2


_unaryOp :: (a->a) -> Pt2 a -> Pt2 a
_unaryOp op p = Pt2 (op x, op y ) where Pt2 (x,y) = p

_xyOp :: (a->a->a) -> Pt2 a -> Pt2 a -> Pt2 a
_xyOp op p1 p2 = Pt2 (op x1 x2, op y1 y2)
  where Pt2 (x1,y1) = p1
        Pt2 (x2,y2) = p2

_scalarOp :: (a -> a -> a) -> Pt2 a -> a -> Pt2 a
_scalarOp op p n = Pt2 (op x n, op y n) where Pt2 (x,y) = p

maxPt2Dist :: (Ord a, Floating a) => [Pt2 a] -> a
maxPt2Dist = foldl' maxDist 0
  where maxDist d p = max (ptDist p) d
        ptDist p = sqrt ( p `dotPt2` p )
