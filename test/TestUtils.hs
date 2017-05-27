module TestUtils (
  binaryTestCase,
  binaryTestCases,
  testFailures,
  polyPtRange,
  randPts
) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Pt2
import           Rand
import           Test.HUnit

binaryTestCase assert methodName apply (arg, expect) =
  TestLabel testName testCase
    where
      testName = methodName ++ " " ++ show arg
      testCase = TestCase $ assert testName expect (apply arg)

binaryTestCases assert name apply = fmap (binaryTestCase assert name apply)

testFailures t = do
  (cnt, det) <- runTestText putTextToShowS t
  putStrLn $ det "--\n"

polyPtRange = (-10::Double,10::Double)

randPts :: Int -> RandomState [Pt2 Double]
randPts n = do
  xs <- randRs polyPtRange n
  ys <- randRs polyPtRange n
  let ps = fmap Pt2 (zip xs ys)
  return ps
