module Feh
( Feh
, Translate
, render
) where

-- First cut at a simple just-sample-me function-style sound constructor.
-- Perhaps with inlining this could be fast?
-- I might also want to add a way to grab entire blocks at a time, for fast
-- mixing, if necessary.

import Sound

class Feh a where
  getRange :: a -> (Int, Int)
  sample :: a -> Int -> (Float, Float)

startOf feh = case getRange feh of (s, e) -> s

instance Feh Sound where
  getRange sound = (0, numFrames sound)
  -- TODO fewer args
  sample sound i = getSample sound i

data Translate a = Translate a Int
instance Feh a => Feh (Translate a) where
  getRange (Translate feh n) = case getRange feh of (s, e) -> (s-n, e-n)
  sample (Translate feh n) i = sample feh (i-n)

render :: Feh a => a -> Sound
render feh | startOf feh /= 0 = render (Translate feh (- (startOf feh)))
render feh | otherwise =
  let (0, e) = getRange feh
      sound = fromSamples $ map (sample feh) (take e [0..])
   in sound
