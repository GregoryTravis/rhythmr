module Main where

import Data.List.Utils (replace)
import qualified Data.StorableVector as SV
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import System.Directory
import System.FilePath.Posix (takeBaseName)

import Analysis
import Arrangement
import Aubio
import Download
import Resample
import Search
import Song
import Sound
import Util

theArrayArrangement =
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

showit vs = mapM putStrLn (map s vs)
  where s ((t0, t1), dx) = (show t0) ++ " " ++ (show dx)

downloadMain searchString count = do
  ids <- search searchString count
  msp "ids"
  msp ids
  filenames <- mapM download ids
  mapM save filenames
  where save filename = do
          msp ("copy", filename, dest filename)
          createDirectoryIfMissing True dir
          copyFile filename (dest filename)
          return $ dest filename
        dir = "tracks/" ++ searchStringDir
        dest filename = dir ++ "/" ++ (takeBaseName filename) ++ ".wav"
        searchStringDir = replace " " "-" searchString

-- anArr = [p]
--   where p = Placement

_main = do
  s0 <- readSound "loop-0-20732.wav"
  s1 <- readSound "loop-1-20732.wav"
  msp $ SV.length (samples s0)
  let arr = Arrangement [Placement s0 (Span 0 b), Placement s1 (Span b (b*2))]
      b = 44100
  song <- renderArrangement arr
  writeSound "hoho.wav" song
  msp "hi"

main = do
  noBuffering
  --ids <- search "drum tracks instrumental" 30
  filenames <- downloadMain "percussion isolated" 20
  let seeds = take 1 $ drop 3 $ take 10 [2885, 8834..]
  msp ("seeds", seeds)
  time "render" $ mapM (renderSong' theArrayArrangement filenames) seeds
  msp "hi"
