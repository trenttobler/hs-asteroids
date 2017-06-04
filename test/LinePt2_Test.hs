module LinePt2_Test (
  linePt2Tests
) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Graphics.Rendering.OpenGL
import           Test.HUnit

import           Rand
import           LinePt2
import           TestUtils

linesCrossedTests = TestLabel "linesCrossed"
    $ TestList testCases'
  where label' (a,b,e) = "linesCrossed " ++ show a
                         ++ " with " ++ show b ++ " = " ++ show e
        testCase' c@(a,b,e) = TestCase $ assertEqual (label' c)
                                         (linesCrossed a b)
                                         e
        testCases' = fmap testCase' testCaseList
        testCaseList = [ (linePt2 0 0 1 1.5, linePt2 1.5 0 0 1, True)
                       , (linePt2 0 0 1 1.5, linePt2 1 0 1 1.0, False) ]

lineCrossPt2Tests = TestLabel "lineCrossPt2"
    $ TestList testCases'
  where label' (a,b,e) = "lineCrossPt2 " ++ show a
                         ++ " with " ++ show b ++ " = " ++ show e
        testCase' c@(a,b,e) = TestCase $ assertEqual (label' c)
                                       (lineCrossPt2 a b)
                                       e
        testCases' = fmap testCase' testCaseList
        testCaseList = [ (linePt2 0 0 1 1, linePt2 1 0 0 1, pt2 0.5 0.5 )
                       , (linePt2 0 0 1 3, linePt2 1 0 0 1, pt2 0.25 0.75 ) ]

pairsA = [ linePt2 1 1 1 2
         , linePt2 2 1 2 2
         , linePt2 3 1 4 2 ]

pairsB = [ linePt2 0 1 7 3
         , linePt2 7 0 0 2 ]

testPairs = sort $ crossedLinePairs pairsA pairsB
testPairs' = sort $ crossedLinePairsBrute pairsA pairsB

crossedLinePairsTests = TestLabel "crossedLinePairsTests" testCase'
  where label' = "crossedLinePairsTests pairsA pairsB"
        testCase' = TestCase $ assertEqual label' testPairs testPairs'

rightTestCase t@(p, line, n) = TestLabel label' $ TestCase $ assertEqual label' n' n
  where label' = show t
        n' = pt2LineRightCrossingNumber p line

rightTestCases = TestLabel "rightCrossingNumber" $ TestList $ fmap rightTestCase rightCrossSamples

rightCrossSamples = -- through point
                    [ (pt2 1 1, linePt2 1 1 1 2, 0)

                    -- degenerate line (point)
                    , (pt2 1 1, linePt2 2 1 2 1, 0)

                    -- entirely above
                    , (pt2 1 1, linePt2 2 3 2 2, 0)
                    , (pt2 1 1, linePt2 2 2 2 3, 0)

                    -- entirely below
                    , (pt2 1 1, linePt2 2 0 2 (-1), 0)
                    , (pt2 1 1, linePt2 2 (-1) 2 0, 0)

                    -- touching and up
                    , (pt2 1 1, linePt2 2 1 1 2, -1)
                    , (pt2 1 1, linePt2 2 1 2 2, -1)
                    , (pt2 1 1, linePt2 2 1 3 2, -1)

                    -- crosses and up
                    , (pt2 1 1, linePt2 2 0 1 2, -1)
                    , (pt2 1 1, linePt2 2 0 2 2, -1)
                    , (pt2 1 1, linePt2 2 0 3 2, -1)

                    -- crosses and down
                    , (pt2 1 1, linePt2 2 2 1 0, 1)
                    , (pt2 1 1, linePt2 2 2 2 0, 1)
                    , (pt2 1 1, linePt2 2 2 3 0, 1)

                    -- touching and up
                    , (pt2 1 1, linePt2 3 2 2 1, 1)
                    , (pt2 1 1, linePt2 2 2 2 1, 1)
                    , (pt2 1 1, linePt2 1 2 2 1, 1)

                    -- touching and down
                    , (pt2 1 1, linePt2 2 1 1 0, 0)
                    , (pt2 1 1, linePt2 2 1 2 0, 0)
                    , (pt2 1 1, linePt2 2 1 3 0, 0)

                    -- entirely left
                    , (pt2 1 4, linePt2 1 1 1 2, 0)
                    , (pt2 1 4, linePt2 2 1 2 1, 0)
                    , (pt2 1 4, linePt2 2 2 2 3, 0)
                    , (pt2 1 4, linePt2 2 3 2 2, 0)
                    , (pt2 1 4, linePt2 2 0 2 (-1), 0)
                    , (pt2 1 4, linePt2 2 (-1) 2 0, 0)
                    , (pt2 1 4, linePt2 2 1 1 2, 0)
                    , (pt2 1 4, linePt2 2 1 2 2, 0)
                    , (pt2 1 4, linePt2 2 1 3 2, 0)
                    , (pt2 1 4, linePt2 2 0 1 2, 0)
                    , (pt2 1 4, linePt2 2 0 2 2, 0)
                    , (pt2 1 4, linePt2 2 0 3 2, 0)
                    , (pt2 1 4, linePt2 2 2 1 0, 0)
                    , (pt2 1 4, linePt2 2 2 2 0, 0)
                    , (pt2 1 4, linePt2 2 2 3 0, 0)
                    , (pt2 1 4, linePt2 3 2 2 1, 0)
                    , (pt2 1 4, linePt2 2 2 2 1, 0)
                    , (pt2 1 4, linePt2 2 1 3 1, 0)
                    , (pt2 1 4, linePt2 2 1 1 0, 0)
                    , (pt2 1 4, linePt2 2 1 2 0, 0)
                    , (pt2 1 4, linePt2 2 1 3 0, 0) ]


linePt2Tests = TestList [ linesCrossedTests
                        , lineCrossPt2Tests
                        , crossedLinePairsTests
                        , rightTestCases ]
