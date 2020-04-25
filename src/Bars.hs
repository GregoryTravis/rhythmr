module Bars
( barsSearch
, barsYouTubeURL
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

doSpleeter = False

searchAndDownloadFiles :: String -> Int -> IO [FilePath]
searchAndDownloadFiles searchString count = do
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
        searchStringDir = searchStringToFilename searchString

searchStringToFilename :: String -> String
searchStringToFilename s = replace ' ' '-' s

barsSearch :: String -> Int -> IO ()
barsSearch searchString numTracks = do
  filenames <- searchAndDownloadFiles searchString 8
  mapM_ extractLoops filenames

barsYouTubeURL :: String -> IO ()
barsYouTubeURL id = do
  --let destFilename = "tracks/" ++ id
  filename <- download id
  --renameFile filename destFilename
  --msp destFilename
  extractLoops filename

extractLoops filename = do
  msp filename
  bars <- fmap (take 40 . drop 10) $ barBeat filename
  original <- readSound filename
  let originalLoops = splitIntoLoops original bars
  originalFilenames <- writeSounds originalLoops
  msp originalFilenames
  if doSpleeter
     then do spleetered <- spleeter original
             let spleeteredLoops = splitIntoLoops spleetered bars
             spleeteredFilenames <- writeSounds spleeteredLoops
             msp spleeteredFilenames
     else return ()
  where writer :: Sound -> String -> IO ()
        writer sound filename = writeSound filename sound
        writeSounds :: [Sound] -> IO [String]
        writeSounds sounds = mapM (contentAddressableWrite fileStub "loops" "wav" . writer) sounds
        fileStub = "loop-" ++ takeBaseName filename

splitIntoLoops :: Sound -> [Int] -> [Sound]
splitIntoLoops sound bars =
  map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))
