module Pt2 (
  Pt2(..), pt2,
  dotPt2,
  crossPt2,
  mulPt2,
  divPt2,
  midPt2
) where

newtype Pt2 a = Pt2 (a,a) deriving Eq

pt2 :: a -> a -> Pt2 a
pt2 x y = Pt2 (x,y)

instance Show a => Show (Pt2 a) where show p = "<Pt2>" ++ show (x,y) where Pt2 (x,y) = p

instance Num a => Num (Pt2 a) where
  (+)             = _xyOp (+)
  (-)             = _xyOp (-)
  (*)             = _xyOp (*)
  abs             = _unaryOp abs
  signum          = _unaryOp signum
  fromInteger i   = Pt2 (fromInteger i,fromInteger i)

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
