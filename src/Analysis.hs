module Analysis
( rms
, rmsTo
, compareRms
, rmsSimilarity ) where

import Data.List.Split (chunksOf)
import qualified Data.StorableVector as SV

import Zounds

-- TODO: also use right channel
rmsWithTimes :: Int -> Zound -> [(Float, Float)]
rmsWithTimes frameSize z = zip times (rms frameSize z)
  where times = map (\s -> (fromIntegral s / (fromIntegral 44100)) * (fromIntegral frameSize)) [0..]

-- rms, but specify the # of chunks instead of the size
rmsTo :: Int -> Zound -> [Float]
rmsTo num sound = rms frameSize sound
  where frameSize | len `mod` num == 0 = len `div` num
                  | otherwise = (len `div` num) + 1
        len = numFrames sound

rms :: Int -> Zound -> [Float]
rms frameSize sound = map rmsFrame (groupIntoFrames leftChannel)
  where leftChannel = map realToFrac $ SV.unpack $ (SV.deinterleave 2 (samples sound)) !! 0
        rmsFrame frame = sqrt (avg (map (**2) frame))
        groupIntoFrames v = chunksOf frameSize v
        avg xs = (sum xs) / (fromIntegral (length xs))

compareRms frameSize s0 s1 =
  let rms0 = rmsWithTimes frameSize s0
      rms1 = rmsWithTimes frameSize s1
   in map diff (zip rms0 rms1)
  where diff ((t0, x0), (t1, x1)) = ((t0, t1), x1-x0)

rmsSimilarity frameSize s0 s1 =
  avg $ map snd $ compareRms frameSize s0 s1
  where avg :: [Float] -> Float
        avg xs = (sum xs) / fromIntegral (length xs)
