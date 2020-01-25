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

reSnip :: Replacer -> Arrangement -> Arrangement
reSnip f (Arrangement [(Placement sound (Span s e))]) = Arrangement [p]
  where p = Placement newSound (Span s e)
        [(newS, newE)] = f (s, e)
        newSound = snip s e sound

halve :: Arrangement -> Arrangement
halve = reSnip f
  where f (s, e) = [(s, halfE)]
          where halfE = s + ((e - s) `div` 2)
