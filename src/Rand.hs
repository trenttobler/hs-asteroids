module Rand (
  seededRandomSeq, mkRandomSeq,

  RandomState, Random,
  runRand, runSeedRand, randR, randRs,

  randPair, randPairs )
where

import           Control.Applicative
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random
import           System.Random.TF

type RandomState = State TFGen

-- | create a new random sequence for a given seed.
seededRandomSeq::Int->TFGen
seededRandomSeq = mkTFGen

-- | create a new random sequence using a random seed.
mkRandomSeq::IO TFGen
mkRandomSeq = newTFGen

runRand :: RandomState a -> IO a
runRand s = fmap (evalState s) mkRandomSeq

runSeedRand :: Int -> RandomState a -> a
runSeedRand seed s = evalState s (seededRandomSeq seed)

randR :: Random a => (a,a) -> RandomState a
randR range = state (randomR range)

randRs :: (Random a) => (a,a) -> Int -> RandomState [a]
randRs range n = replicateM n (randR range)

randPair :: Random a => (a,a) -> RandomState (a,a)
randPair r = liftA2 (,) rndC rndC where rndC = randR r

randPairs :: Random a => (a,a) -> Int -> RandomState [(a,a)]
randPairs r n = replicateM n (randPair r)
