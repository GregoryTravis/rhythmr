module FX
( FX(..)
, applyFX ) where

import External
import Sound
import Util

data FX = NoFX | Highpass Int | Lowpass Int
  deriving Show

--ssRun :: (String -> String -> [String]) -> (Sound -> IO Sound)
ssRun :: [String] -> (Sound -> IO Sound)
ssRun subcommand sound = runViaFilesCmd "wav" writeSound readSound commander sound
  where commander = \s d -> ["sox", "-G", s, d] ++ subcommand

applyFX :: FX -> Sound -> IO Sound

applyFS NoFX s = return s

applyFX (Highpass freq) s = ssRun ["highpass", "-2", show freq] s
applyFX (Lowpass freq) s = ssRun ["lowpass", "-2", show freq] s

applyFX x s = return $ eesp (show ("um", x)) s
