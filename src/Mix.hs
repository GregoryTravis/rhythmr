module Mix
( Mix(..)
, mixdown ) where

import Sound

data Mix = ASound Sound | Par [Mix] | Seq [Mix]

mixdown :: Mix -> Sound
mixdown (ASound sound) = sound
mixdown (Par mixes) = mixSounds (map mixdown mixes)
mixdown (Seq mixes) = appendSounds (map mixdown mixes)
