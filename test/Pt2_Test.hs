module Pt2_Test (
  pt2Tests
) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Graphics.Rendering.OpenGL
import           Test.HUnit

import           Rand
import           PolyPt2
import           Pt2
import           TriPt2
import           TestUtils

-- Test cases for point calculations
data PtCase = PtCase
  { ptA::Pt2 Double
  , ptB::Pt2 Double
  , ptSum::Pt2 Double
  , ptDif::Pt2 Double
  , ptMid::Pt2 Double
  , ptDot::Double
  , ptCross::Double }
  deriving Show
ptCase (ptA, ptB, ptSum, ptDiff, ptMid, ptDot, ptCross) = PtCase ptA ptB ptSum ptDiff ptMid ptDot ptCross

pt2TestSamples =
  fmap ptCase
    --   ptA          ptB             ptSum         ptDiff        ptMid           ptDot  ptCross )
    --  ------------- -------------   -----------   ------------- --------------- ------ --------
    [ (  Pt2 (3,5)   , Pt2 (1,2)     , Pt2 (4,7)   , Pt2 (2,3)   , Pt2 (2,3.5)   , 13   ,  1     ),
      (  Pt2 (0,1)   , Pt2 (1,0)     , Pt2 (1,1)   , Pt2 (-1,1)  , Pt2 (0.5,0.5) ,  0   , -1     ),
      (  Pt2 (1,1)   , Pt2 (-1,-3)   , Pt2 (0,-2)  , Pt2 (2,4)   , Pt2 (0,-1)    , -4   , -2     )
      ]

pt2Name p = show (ptA p) ++ " and " ++ show (ptB p)
pt2Test p = TestLabel (pt2Name p) $ TestList [
  TestCase $ assertEqual "(+)" (ptSum p) (ptA p + ptB p),
  TestCase $ assertEqual "(-)" (ptDif p) (ptA p - ptB p),
  TestCase $ assertEqual "midPt2" (ptMid p) (midPt2 (ptA p) (ptB p)),
  TestCase $ assertEqual "dotPt2" (ptDot p) (dotPt2 (ptA p) (ptB p)),
  TestCase $ assertEqual "crossPt2" (ptCross p) (crossPt2 (ptA p) (ptB p))
  ]
  
pt2Tests = TestList $ fmap pt2Test pt2TestSamples
