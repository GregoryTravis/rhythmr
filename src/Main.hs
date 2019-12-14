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
  , [0, 3]
  , [0, 1, 2, 3]
  , [0, 1, 2, 3] ]

main = do
  noBuffering
  ids <- search
  msp ids
  --let ids' = [ids !! 0]
  let seeds = take 3 [124..]
  filenames <- mapM download ids
  msp filenames
  mapM (renderSequence theSequence filenames) seeds
  msp "hi"
