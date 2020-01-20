module FX
( FX(..)
, applyFX ) where

import External
import Util

data FX = Highpass freq

applyFX :: FX -> Sound -> Sound
applyFX (Highpass freq) s = return s
