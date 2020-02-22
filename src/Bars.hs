module Bars
  ( bars
  ) where

import System.Directory
import System.FilePath.Posix (takeBaseName)

import Aubio
import Download
import External (contentAddressableWrite)
import Search
import Sound
import Spleeter
import Util

downloadFiles searchString count = do
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
        searchStringDir = replace ' ' '-' searchString

bars :: String -> Int -> IO ()
bars searchString numTracks = do
  filenames <- downloadFiles searchString 8
  mapM_ extractLoops filenames

extractLoops filename = do
  msp filename
  bars <- fmap (take 8 . drop 10) $ barBeat filename
  original <- readSound filename
  let originalLoops = splitIntoLoops original bars
  originalFilenames <- writeSounds originalLoops
  msp originalFilenames
  spleetered <- spleeter original
  let spleeteredLoops = splitIntoLoops spleetered bars
  spleeteredFilenames <- writeSounds spleeteredLoops
  msp spleeteredFilenames
  where writer :: Sound -> String -> IO ()
        writer sound filename = writeSound filename sound
        writeSounds :: [Sound] -> IO [String]
        writeSounds sounds = mapM (contentAddressableWrite "loop" "loops" "wav" . writer) sounds

splitIntoLoops :: Sound -> [Int] -> [Sound]
splitIntoLoops sound bars =
  map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))
