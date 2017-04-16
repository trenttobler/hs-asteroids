module TestUtils (
  binaryTestCase,
  binaryTestCases,
  testFailures
) where

import           Control.Monad
import           Data.Monoid
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
