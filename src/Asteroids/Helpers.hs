module Asteroids.Helpers (
  module Asteroids.Helpers
)
where

import Data.List
import Data.Fixed
 
fillIn :: (a->[b]) -> [b] -> [a] -> [b]
fillIn f sep parts = intercalate sep $ fmap f parts

showLabels :: String -> [(String, t -> String)] -> t -> String
showLabels partSep parts p = fillIn showPart partSep parts
  where showPart (label, attr) = pad label ++ attr p
        len = maximum $ fmap (length . fst) parts
        pad s = s ++ replicate (len - length s) ' '

between :: Ord a => (a, a) -> a -> Bool
between (lower,upper) val = lower <= val && val <= upper

modularInterval :: Real a => (a, a) -> a -> a
modularInterval (lower,upper) x =
  if between (lower,upper) x
  then x
  else ( (x - lower) `mod'` (upper - lower)) + lower

fromPolar :: Floating t => t -> t -> (t, t)
fromPolar d a = (d * cos a, d * sin a)

wrappedPairs :: [a] -> [(a,a)]
wrappedPairs [] = []
wrappedPairs xs = pairs x x (tail xs) []
  where x = head xs
        pairs :: x -> x -> [x] -> [(x,x)] -> [(x,x)]
        pairs q' p qs pqs
          | pqs `seq` False = undefined
          | null qs = (p,q'):pqs
          | otherwise = let q = head qs
                            qs' = tail qs
                            pqs' = (p, q) : pqs
                        in pairs q' q qs' (seq pqs' pqs')
