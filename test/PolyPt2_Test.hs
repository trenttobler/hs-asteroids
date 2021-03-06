module PolyPt2_Test (
  polyPt2Tests
) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Graphics.Rendering.OpenGL
import           System.Random
import           Test.HUnit

import           Rand
import           PolyPt2
import           Pt2
import           TriPt2
import           TestUtils

-- Test cases for triangle calculations.
data TriCase = TriCase 
  { triX::TriPt2 Double
  , triMid::Pt2 Double
  , triArea::Double }
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
nextPoly :: Int -> RandomState (PolyPt2 Double, Double, String)
nextPoly n = do
  pts <- randPts n
  return (pts, fromIntegral n, "poly#" ++ show n)

randPolys :: RandomState [(PolyPt2 Double, Double, String)]
randPolys = do
  p3 <- nextPoly 3
  p4 <- nextPoly 4
  -- ps <- sequence $ mapM nextPoly [3..100::Int]
  let ps = [p3, p4]
  return ps

testPolys :: [(PolyPt2 Double, Double, String)]
testPolys = runSeedRand 1 randPolys

getTestPoly :: Int -> (PolyPt2 Double, Double, String)
getTestPoly n = head $ filter isN testPolys
  where
    isN (p,sz,name) = name == "poly#" ++ show n

testPoly :: Int -> IO ()
testPoly n = do
  let (p,sz,name) = getTestPoly n
  let (area,pp) = polyAreaMidPt2 p
  let midQ = dotPt2 pp pp
  putStrLn name
  putStrLn $ show (length p) ++ " points"
  putStrLn $ show area ++ " should be " ++ show sz ++ " (diff: " ++ show ( abs $ area - sz ) ++ ")"
  putStrLn $ "midpoint " ++ show pp ++ ",should be 0 (dist: " ++ show ( sqrt midQ ) ++ ")"
  putStrLn $ "Polygon is : " ++ show p

polyEpsilon = 1e-10
polyNormPt2Test (p,sz,name) = TestLabel name
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


testIPoly = fmap Pt2 [ (2,0), (2,8), (4,8), (4,2), (6,2)
                     , (6,4), (0,4), (0,6), (8,6), (8,0) ]

-- testIPoly = fmap Pt2 [ (2,0), (8,0), (8,6), (0,6), (0,4)
--                      , (6,4), (6,2), (4,2), (4,8), (2,8) ]

alternatingPts = [        (3,1), (5,1), (7,1)
                 ,        (3,3),        (7,3)
                 , (1,5),        (5,5), (7,5)
                 ,        (3,7) ]
windingPts = (3,5):alternatingPts
samplePts = [(2 * x - 1, 2 * y - 1) | x <- [0..5], y <- [0..5]]

intersectCases trueName truePts inside = fmap testCase' samplePts
  where testCase' p = TestLabel (label' p) $ TestCase
                    $ assertEqual (show p)
                        (p `elem` truePts)
                        (inside (Pt2 p) testIPoly)
        label' p = trueName ++ " " ++ show p

polyPt2Tests = TestList [ TestList polyNormTestCases
                        , TestList triPt2Tests
                        , TestList $ intersectCases "winding"
                                        windingPts pt2InsideWindingPoly
                        , TestList $ intersectCases "alternate"
                                        alternatingPts pt2InsideAlternatingPoly]

