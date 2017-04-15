module Rand (
    seededRandomSeq,
    getRandomSeed,
    mkRandomSeq )
where

import System.Random.TF
import Data.Time.Clock.POSIX

-- | create a new random sequence for a given seed.
seededRandomSeq::Int->TFGen
seededRandomSeq = mkTFGen

-- | get an integer value suitable for use in a non-cryptographic random number generator.
getRandomSeed::IO Int
getRandomSeed = do
    t <- getPOSIXTime
    return $ fromIntegral $ round $ 1000 * realToFrac t

-- | create a new random sequence using a random seed.
mkRandomSeq::IO TFGen
mkRandomSeq = do
    s <- getRandomSeed
    return $ seededRandomSeq s
