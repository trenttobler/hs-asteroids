import           Control.Monad
import           Data.Monoid
import           Test.HUnit

import           Pt2_Test
import           PolyPt2_Test
import           LinePt2_Test
import           TestUtils

tests = TestList [
    polyPt2Tests,
    pt2Tests,
    linePt2Tests ]

main :: IO Counts
main = runTestTT tests



