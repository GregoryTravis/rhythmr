module Dice
( double
, halve ) where

import Arrangement
import Sound
import Util

-- This stuff should really be working with arrangements

-- Input must be an Arrangement of just one sound
type Replacer = (Int, Int) -> [(Int, Int)]
rePlace :: Replacer -> Arrangement -> Arrangement
rePlace f (Arrangement [(Placement sound (Span s e))]) = Arrangement (map (uncurry place) (zip ses (repeat sound)))
  where ses = f (s, e)
        place (s, e) sound = Placement sound (Span s e)

double :: Arrangement -> Arrangement
double = rePlace f
  where f (s, e) = [(s, mid), (mid, e)]
          where mid = (s + e) `div` 2

halve :: Arrangement -> Arrangement
--halve arr f = rePlace arr (\

halve (Arrangement [(Placement sound (Span s e))]) = Arrangement [p]
  where halfSound = snip s halfE sound
        halfE = s + ((e - s) `div` 2)
        p = Placement halfSound (Span s e)
