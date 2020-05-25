module Resample (resampleZound) where

import System.Directory
import System.IO.Temp

import External
import Zounds
import Util

resampleZound :: Int -> Zound -> IO Zound
resampleZound destLengthFrames sound = do
  runViaFilesCmd "wav" writeZound readZound resample sound
  where resample src dest = ["sox", "-G", src, dest, "speed", show speedRatio]
        speedRatio = (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
        srcLengthFrames = numFrames sound
