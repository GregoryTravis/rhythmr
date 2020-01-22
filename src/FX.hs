module FX
( FX(..)
, applyFX ) where

import Constants
import Data.List (intercalate)
import External
import Sound
import Util

data FX = NoFX | Highpass Int | Lowpass Int | Chorus | Band Int Int | NoiseGate | Squelch | Echo Int | Flange | MCompand | Overdrive Int Int | Phaser | Pitch Int | Reverb
  deriving Show

--ssRun :: (String -> String -> [String]) -> (Sound -> IO Sound)
ssRun :: [String] -> (Sound -> IO Sound)
ssRun subcommand sound = runViaFilesCmd "wav" writeSound readSound commander sound
  where commander = \s d -> ["sox", "-G", s, d] ++ subcommand

applyFX :: FX -> Sound -> IO Sound

applyFX NoFX = return

applyFX (Highpass freq) = ssRun ["highpass", "-2", show freq]
applyFX (Lowpass freq) = ssRun ["lowpass", "-2", show freq]

applyFX Chorus = ssRun ["chorus", "0.7", "0.9", "55", "0.4", "0.25", "2", "-t", "60", "0.32", "0.4", "2.3", "-t", "40", "0.3", "0.3", "1.3", "-s"]

applyFX (Band center width) = ssRun ["band", "-n", show center, show width]
applyFX NoiseGate = ssRun ["compand", ".1,.2", "-inf,-50.1,-inf,-50,-50", "0", "-90", ".1"]
applyFX Squelch = ssRun ["compand", ".1,.1", "-40.1,-40,-inf,0,-inf", "35", "-90", ".1"]

applyFX (Echo n) = ssRun ["echos", "1.0", "1.0", show delay, "0.3"]
  where delay = toMS $ loopLengthSeconds / (fromIntegral n)
        toMS s = floor (1000.0 * s)

applyFX Flange = ssRun ["flanger"]

applyFX MCompand = ssRun ["mcompand",
  "0.005,0.1 -47,-40,-34,-34,-17,-33", "100", 
  "0.003,0.05 -47,-40,-34,-34,-17,-33", "400", 
  "0.000625,0.0125 -47,-40,-34,-34,-15,-33", "1600", 
  "0.0001,0.025 -47,-40,-34,-34,-31,-31,-0,-30", "6400", 
  "0,0.025 -38,-31,-28,-28,-0,-25"]

applyFX (Overdrive gain colour) = ssRun ["overdrive", show gain, show colour]

applyFX Phaser = ssRun ["phaser", "0.89", "0.85", "1", "0.24", "2", "-t"]
-- more
-- ["phaser", "0.6", "0.66", "3", "0.6", "2", "-t"]

applyFX (Pitch cents) = ssRun ["pitch", show cents]
applyFX Reverb = ssRun ["reverb"]
