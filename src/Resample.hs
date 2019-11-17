module Resample (resampleSound) where

import System.Directory
import System.IO.Temp
import System.Process

import Sound
import Util

resampleSound :: Int -> Sound -> IO Sound
resampleSound destLengthFrames sound = do
  tmpSrc <- emptySystemTempFile "src.wav"
  tmpDest <- emptySystemTempFile "dest.wav"
  writeSound tmpSrc sound
  callProcess "/usr/local/bin/sox" [tmpSrc, tmpDest, "speed", show speedRatio]
  dest <- readSound tmpDest
  removeFile tmpSrc
  removeFile tmpDest
  return dest
  where speedRatio = (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
        srcLengthFrames = numFrames sound
