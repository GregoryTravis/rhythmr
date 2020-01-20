module FX
( FX(..)
, applyFX ) where

import External
import Sound
import Util

data FX = Highpass Int

--ssRun :: (String -> String -> [String]) -> (Sound -> IO Sound)
ssRun :: [String] -> (Sound -> IO Sound)
ssRun subcommand sound = runViaFilesCmd "wav" writeSound readSound commander sound
  where commander = \s d -> ["sox", "-G", s, d] ++ subcommand

applyFX :: FX -> Sound -> IO Sound
applyFX (Highpass freq) = ssRun ["highpass", "-2", show freq]
