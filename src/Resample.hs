module Resample (resampleSound) where

import System.Directory
import System.IO.Temp

import External
import Sound
import Util

resampleSound :: Int -> Sound -> IO Sound
resampleSound destLengthFrames sound = do
  runViaFiles "wav" writeSound readSound resample sound
  where resample src dest = runProc "sox" ["-G", src, dest, "speed", show speedRatio]
        speedRatio = (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
        srcLengthFrames = numFrames sound
