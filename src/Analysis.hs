module Analysis
( rms
, compareRms
, rmsSimilarity) where

import Data.List.Split (chunksOf)
import qualified Data.StorableVector as SV

import Sound

-- TODO: also use right channel
--rms :: Int -> Sound -> [(Double)]
rms frameSize sound = zip times $ map rmsFrame (groupIntoFrames leftChannel)
  where leftChannel = map realToFrac $ SV.unpack $ (SV.deinterleave 2 (samples sound)) !! 0
        rmsFrame frame = sqrt (avg (map (**2) frame))
        groupIntoFrames v = chunksOf frameSize v
        avg xs = (sum xs) / (fromIntegral (length xs))
        times = map (\s -> (fromIntegral s / (fromIntegral 44100)) * (fromIntegral frameSize)) [0..]

compareRms frameSize s0 s1 =
  let rms0 = rms frameSize s0
      rms1 = rms frameSize s1
   in map diff (zip rms0 rms1)
  where diff ((t0, x0), (t1, x1)) = ((t0, t1), x1-x0)

rmsSimilarity frameSize s0 s1 =
  avg $ map snd $ compareRms frameSize s0 s1
  where avg :: [Float] -> Float
        avg xs = (sum xs) / fromIntegral (length xs)
