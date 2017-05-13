module Pt2 (
  Pt2(..),
  dotPt2,
  crossPt2,
  mulPt2,
  divPt2,
  midPt2
) where

newtype Pt2 = Pt2 (Double,Double) deriving Eq

instance Show Pt2 where show p = "<Pt2>" ++ show (x,y) where Pt2 (x,y) = p

instance Num Pt2 where
  (+)             = _xyOp (+)
  (-)             = _xyOp (-)
  (*)             = _xyOp (*)
  abs             = _unaryOp abs
  signum          = _unaryOp signum
  fromInteger i   = Pt2 (fromInteger i,fromInteger i)

mulPt2 :: Pt2 -> Double -> Pt2
mulPt2            = _scalarOp (*)

divPt2 :: Pt2 -> Double -> Pt2
divPt2            = _scalarOp (/)

dotPt2 :: Pt2 -> Pt2 -> Double
dotPt2 p1 p2 = xx + yy  where Pt2 (xx,yy) = p1 * p2

crossPt2 :: Pt2 -> Pt2 -> Double
crossPt2 p1 p2 = x1 * y2 - y1 * x2
  where Pt2 (x1,y1) = p1
        Pt2 (x2,y2) = p2

midPt2 :: Pt2 -> Pt2 -> Pt2
midPt2    a b  = divPt2 (a + b) 2


_unaryOp :: (Double->Double) -> Pt2 -> Pt2
_unaryOp op p = Pt2 (op x, op y ) where Pt2 (x,y) = p

_xyOp :: (Double->Double->Double) -> Pt2 -> Pt2 -> Pt2
_xyOp op p1 p2 = Pt2 (op x1 x2, op y1 y2)
  where Pt2 (x1,y1) = p1
        Pt2 (x2,y2) = p2

_scalarOp :: (Double->Double->Double) -> Pt2 -> Double -> Pt2
_scalarOp op p n = Pt2 (op x n, op y n) where Pt2 (x,y) = p
