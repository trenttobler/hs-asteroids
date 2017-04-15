module Shapes_Test (
  shapesTests
) where

import Test.HUnit
import Data.Monoid
import Data.List
import Control.Monad
import System.Random
import Graphics.Rendering.OpenGL

import TestUtils
import Shapes
import Rand

shapesTests = TestList [
  TestList polyNormTestCases,
--  TestList pt2Tests,
  TestList triPt2Tests
  ]

-- Test setup
rand = seededRandomSeq 1

polyPtRange = (-10::Double,10::Double)

randPts r n = (ps,r'')
  where
    (rx,r') = split r
    (ry,r'') = split r'
    xs = randomRs polyPtRange rx
    ys = randomRs polyPtRange ry
    ps = fmap Pt2 $ take n $ zip xs ys


-- Test cases for point calculations
data PtCase = PtCase { ptA::Pt2, ptB::Pt2, ptSum::Pt2, ptDif::Pt2, ptMid::Pt2, ptDot::Double, ptCross::Double }
  deriving Show
ptCase (ptA, ptB, ptSum, ptDiff, ptMid, ptDot, ptCross) = PtCase ptA ptB ptSum ptDiff ptMid ptDot ptCross
pt2TestSamples =
  fmap ptCase
    --   ptA          ptB             ptSum         ptDiff        ptMid           ptDot  ptCross )
    --  ------------- -------------   -----------   ------------- --------------- ------ --------
    [ (  Pt2 (3,5)   , Pt2 (1,2)     , Pt2 (4,7)   , Pt2 (2,3)   , Pt2 (2,3.5)   , 13   ,  1     ),
      (  Pt2 (0,1)   , Pt2 (1,0)     , Pt2 (1,1)   , Pt2 (-1,1)  , Pt2 (0.5,0.5) ,  0   ,  1     ),
      (  Pt2 (1,1)   , Pt2 (-1,-3)   , Pt2 (0,-2)  , Pt2 (2,4)   , Pt2 (0,-1)    , -4   , -2     )
      ]

pt2Name p = show (ptA p) ++ " TO " ++ show (ptB p)
pt2Test p = TestLabel (pt2Name p) $ TestList [
  TestCase $ assertEqual "(+)" (ptSum p) (ptA p + ptB p),
  TestCase $ assertEqual "(-)" (ptDif p) (ptA p - ptB p),
  TestCase $ assertEqual "midPt2" (ptMid p) (midPt2 (ptA p) (ptB p)),
  TestCase $ assertEqual "dotPt2" (ptDot p) (dotPt2 (ptA p) (ptB p)),
  TestCase $ assertEqual "midPt2" (ptCross p) (crossPt2 (ptA p) (ptB p))
  ]
pt2Tests = fmap pt2Test pt2TestSamples

-- Test cases for triangle calculations.
data TriCase = TriCase { triX::TriPt2, triMid::Pt2, triArea::Double }
  deriving Show
triTestSamples = [
  TriCase (TriPt2 (Pt2 (0,0), Pt2 (1,3), Pt2 (2,0))) (Pt2 (1,1)) 3,
  TriCase (TriPt2 (Pt2 (0,0), Pt2 (2,6), Pt2 (4,0))) (Pt2 (2,2)) 12 ]

triPt2Tests = concatMap getCases triTestSamples
  where
    getCases (TriCase triX triMid triArea ) = [
      TestCase $ assertEqual ("triMidPt2 $ " ++ show triX ) triMid $ triMidPt2 triX,
      TestCase $ assertEqual ("triAreaPt2 $ " ++ show triX ) triArea $ triAreaPt2 triX,
      TestCase $ assertEqual ("triAreaMidPt2 $ " ++ show triX ) (triArea,triMid) $ triAreaMidPt2 triX
      ]


-- Test cases for polygon calculations.
nextPoly (ps,r) n = ((p, sz, name) : ps, r')
  where
    (p,r1) = randPts r n
    (sz,r') = randomR (-10.0, 10.0) r1
    name = "poly#" ++ show n

testPolys = fst $ foldl' nextPoly ([],rand) [3..100]

getTestPoly n = head $ filter isN testPolys
  where
    isN (p,sz,name) = name == "poly#" ++ show n

testPoly n = do
  let (poly,sz,name) = getTestPoly n
  let p = polyNormPt2 sz poly
  let (area,pp) = polyAreaMidPt2 p
  let midQ = dotPt2 pp pp
  putStrLn name
  putStrLn $ show (length poly) ++ " points"
  putStrLn $ show area ++ " should be " ++ show sz ++ " (diff: " ++ show ( abs $ area - sz ) ++ ")"
  putStrLn $ "midpoint " ++ show pp ++ ",should be 0 (dist: " ++ show ( sqrt midQ ) ++ ")"
  putStrLn $ "Original Area was: " ++ show (polyAreaPt2 poly)
  putStrLn $ "Polygon is : " ++ show poly

polyEpsilon = 1e-10
polyNormPt2Test (p, sz, name) = TestLabel name
  $ TestList [
    TestCase $ assertBool areaMessage aOk,
    TestCase $ assertBool midMessage midOk ]
  where
    (area, mid) = polyAreaMidPt2 $ polyNormPt2 sz p
    aOk = abs (sz - area) < polyEpsilon
    midOk = abs (dotPt2 mid mid) < polyEpsilon
    areaMessage = "Area " ++ show area ++ " must be close to " ++ show sz
    midMessage = "Midpoint " ++ show mid ++ " must be close to 0"

polyNormTestCases = fmap polyNormPt2Test testPolys
