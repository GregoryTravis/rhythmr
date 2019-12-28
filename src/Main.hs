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

toTiledArrangement :: [[Int]] -> TiledArrangement Int
toTiledArrangement nses = Seq (map (\ns -> Par (map Elem ns)) nses)
theTiledArrangement = toTiledArrangement
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

main = do
  noBuffering
  --ids <- search "drum tracks instrumental" 30
  filenames <- downloadMain "percussion isolated" 20
  let seeds = take 1 $ drop 3 $ take 10 [2885, 8834..]
  msp ("seeds", seeds)
  mapM (renderSong theTiledArrangement filenames) seeds
  msp "hi"
