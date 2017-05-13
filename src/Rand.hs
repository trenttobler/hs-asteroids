module Rand (
    seededRandomSeq,
    getRandomSeed,
    mkRandomSeq,
    randomSeq )
where

import           Data.Bits
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.Random
import           System.Random.TF

-- | create a new random sequence for a given seed.
seededRandomSeq::Int->TFGen
seededRandomSeq = mkTFGen

-- | get an integer value suitable for use in a non-cryptographic random number generator.
getRandomSeed::IO Int
getRandomSeed = do
    t <- getCurrentTime
    let lsb = truncate (utctDayTime t * 1000000)::Int
    let d = diffDays (utctDay t) $ fromGregorian 1970 1 1
    let msb = fromInteger d::Int
    let seed = lsb `xor` msb
    return seed

-- | create a new random sequence using a random seed.
mkRandomSeq::IO TFGen
mkRandomSeq = do
    s <- getRandomSeed
    return $ seededRandomSeq s

randomSeq :: (Random a, RandomGen g) => Int -> (a, a) -> g -> [a]
randomSeq n (a,b) r = take n $ randomRs (a,b) r
