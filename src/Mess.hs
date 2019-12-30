module Mess
( scrunch
) where

import Arrangement
import Sound
import Util

scrunch :: Int -> Double -> Sound -> Arrangement
scrunch numPieces scrunchFactor sound =
  let len = numFrames sound
      starts = flip map (take (numPieces+1) [0..]) $ \i -> (i * len) `div` numPieces
      startEnds = take numPieces $ zip starts (tail starts)
      unscrunched = Arrangement $ flip map (zip (repeat sound) startEnds) $ \(sound, (s, e)) -> Placement (snip s e sound) (Span s e)
      scrunched = mapSpans (scrunchSpan scrunchFactor) unscrunched
      scrunchSpan scrunchFactor (Span s e) = Span (s-dx) (e-dx)
        where dx = (floor (fromIntegral s * (1-scrunchFactor)))
   in eesp unscrunched $ eesp scrunched $ scrunched
   --in eesp unscrunched $ scrunched
   --in scrunched
