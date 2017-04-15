import Test.HUnit
import Data.Monoid
import Control.Monad

import Shapes_Test
import TestUtils

tests = TestList [
    shapesTests ]

main :: IO Counts
main = runTestTT tests



