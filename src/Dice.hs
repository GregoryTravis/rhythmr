module Dice
( double
, halve ) where

import Arrangement
import Sound
import Util

-- This stuff should really be working with arrangements

double :: Arrangement -> Arrangement
double (Arrangement [(Placement sound (Span s e))]) = Arrangement [p0, p1]
  where p0 = Placement sound (Span s0 e0)
        p1 = Placement sound (Span s1 e1)
        s0 = s
        e0 = mid
        s1 = mid
        e1 = e
        mid = (s + e) `div` 2

halve :: Arrangement -> Arrangement
halve (Arrangement [(Placement sound (Span s e))]) = Arrangement [p]
  where halfSound = snip s halfE sound
        halfE = s + ((e - s) `div` 2)
        p = Placement halfSound (Span s e)
