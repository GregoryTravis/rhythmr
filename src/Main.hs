module Main where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
--import System.IO
--import System.Process

import Aubio
import Download
import Resample
import Search
import Sequence
import Sound
import Util

toSequence :: [[Int]] -> Sequence Int
toSequence nses = Seq (map (\ns -> Par (map Elem ns)) nses)
--theSequence = Seq [Par [Elem 1], Par [Elem 1, Elem 2]]
theSequence = toSequence
  [ [0]
  , [0, 1]
  , [0, 2]
  , [0, 3] -- m 6
  , [0, 1, 2, 3]
  , [0, 1, 2, 3]
  , [4]
  , [4, 5] -- m 14
  , [4, 5, 0]
  , [4, 5, 2]
  , [2, 3, 4, 6] -- m
  , [0, 1, 2, 3, 4, 5, 6] -- m
  , [0, 1, 2, 3, 4, 5, 6] ] -- m

main = do
  noBuffering
  --ids <- search "percussion track" 20
  ids <- searchNoPaging "drum breaks" 20
  msp ids
  --let ids' = [ids !! 0]
  let seeds = drop 4 $ take 10 [85, 834..]
  msp ("seeds", seeds)
  filenames <- mapM download ids
  --msp filenames
  mapM (renderSequence theSequence filenames) seeds
  msp "hi"
