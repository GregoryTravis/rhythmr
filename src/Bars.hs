module Bars
( barsSearch
, barsYouTubeURL
) where

import Data.List (stripPrefix)
import System.Directory
import System.FilePath.Posix (takeBaseName)

import Aubio
import Download
import External (contentAddressableWrite)
import Search
import Zounds
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

youtubeUrlPrefix = "https://www.youtube.com/watch?v="

toId :: String -> String
toId s = case stripPrefix youtubeUrlPrefix s of Just s -> s
                                                Nothing -> s

-- takes a url or id
barsYouTubeURL :: String -> IO ()
barsYouTubeURL idOrUrl = do
  --let destFilename = "tracks/" ++ id
  filename <- download (toId idOrUrl)
  --renameFile filename destFilename
  --msp destFilename
  extractLoops filename

extractLoops filename = do
  msp filename
  bars <- fmap (take 40 . drop 10) $ barBeat filename
  original <- readZound filename
  let originalLoops = splitIntoLoops original bars
  originalFilenames <- writeZounds originalLoops
  msp originalFilenames
  if doSpleeter
     then do spleetered <- spleeter original
             let spleeteredLoops = splitIntoLoops spleetered bars
             spleeteredFilenames <- writeZounds spleeteredLoops
             msp spleeteredFilenames
     else return ()
  where writer :: Zound -> String -> IO ()
        writer sound filename = writeZound filename sound
        writeZounds :: [Zound] -> IO [String]
        writeZounds sounds = mapM (contentAddressableWrite fileStub "loops" "wav" . writer) sounds
        fileStub = "loop-" ++ takeBaseName filename

splitIntoLoops :: Zound -> [Int] -> [Zound]
splitIntoLoops sound bars =
  map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))
