module Rand (
    seededRandomSeq,
    getRandomSeed,
    mkRandomSeq )
where

import System.Random.TF
import Data.Time.Clock
import Data.Time.Calendar
import Data.Bits

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
