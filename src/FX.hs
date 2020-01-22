module FX
( FX(..)
, applyFX ) where

import Constants
import Data.List (intercalate)
import External
import Sound
import Util

data FX = NoFX | Highpass Int | Lowpass Int | Chorus | Band Int Int | NoiseGate | Squelch | Echo Int
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
-- n per loop
--applyFX (Echo n) = ssRun (["echos", "1.0", "1.0"] ++ delayDecayPairs)
--  where delayDecayPairs = concat (map p2a (zip delays (repeat (show 0.3))))
--        delays = map delay (take (n-1) [1..])
--        delay i = toMS $ (loopLengthSeconds * (fromInteger i)) / (fromIntegral n)
--        toMS s = floor (1000.0 * s)
--        --pShow (a, b) = show a ++ " " ++ show b
--        p2a (a, b) = [show a, show b]
