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

linePt2Tests = TestList [ linesCrossedTests
                        , lineCrossPt2Tests
                        , crossedLinePairsTests ]
