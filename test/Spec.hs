import           Control.Monad
import           Data.Monoid
import           Test.HUnit

import           Shapes_Test
import           TestUtils

tests = TestList [
    shapesTests ]

main :: IO Counts
main = runTestTT tests



