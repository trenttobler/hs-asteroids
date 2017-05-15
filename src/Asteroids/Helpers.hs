module Asteroids.Helpers (
  fillIn,
  showLabels,
  between,
  modularInterval
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
