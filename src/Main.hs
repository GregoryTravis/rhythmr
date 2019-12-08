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
  [ [8]
  , [8, 2]
  , [8, 2, 3]
  , [8, 2, 3, 4]
  , [8, 3]
  , [2, 4]
  , [2, 5, 6]
  , [2, 5, 7]
  , [1] ]

main = do
  ids <- search
  msp ids
  --let ids' = [ids !! 0]
  filenames <- mapM download ids
  msp filenames
  renderSequence theSequence filenames
  msp "hi"
