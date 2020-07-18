module Bars
( barsSearch
, barsId
, barsIdFile
, barsFile
) where

import Data.List (stripPrefix)
import System.Directory
import System.Directory.Recursive
import System.FilePath.Posix (takeBaseName)

import Aubio
import Download
import External (contentAddressableWrite)
import Search
import Zounds
import Spleeter
import Util

doSpleeter = False

searchAndDownloadFiles :: String -> String -> Int -> IO [FilePath]
searchAndDownloadFiles collection searchString count = do
  -- The take here is because we can get more than we asked for
  ids <- fmap (take count) $ search searchString count
  msp ("ids", ids)
  filenames <- mapM download ids
  return filenames
  -- mapM save filenames
  -- where save filename = do
  --         msp ("copy", filename, dest filename)
  --         copyFile filename (dest filename)
  --         return $ dest filename
  --       dest filename = collection ++ "/" ++ (takeBaseName filename) ++ ".wav"

barsSearch :: String -> String -> Int -> IO ()
barsSearch collection searchString numTracks = do
  filenames <- searchAndDownloadFiles collection searchString 8
  mapM_ (extractLoops collection) filenames

youtubeUrlPrefix = "https://www.youtube.com/watch?v="

toId :: String -> String
toId s = case stripPrefix youtubeUrlPrefix s of Just s -> s
                                                Nothing -> s

-- Download from youtube by IDs or URLs ; possibly remove files after
-- extracting loops.
barsId :: String -> String -> IO ()
barsId collection idOrUrl = do
  --let destFilename = "tracks/" ++ id
  filename <- download (toId idOrUrl)
  --renameFile filename destFilename
  --msp destFilename
  extractLoops collection filename

-- Download from youtube by IDs or URLs listed in files; possibly remove files
-- after extracting loops.
barsIdFile :: String -> [String] -> IO ()
barsIdFile collection filenames = do
  fileContentses <- mapM readFile filenames
  let ids = concat (map lines fileContentses)
  mapM_ (barsId collection) ids

-- Extract loops from existing files or dirs; do not delete the files
barsFile :: String -> [String] -> IO ()
barsFile collection filenames = mapM_ (barsFile1 collection) filenames

-- Extract loops from existing files or dirs; do not delete the files
barsFile1 :: String -> String -> IO ()
barsFile1 collection filename = do
  isDir <- doesDirectoryExist filename
  files <- if isDir then getDirRecursive filename
                    else return [filename]
  mapM_ (extractLoops collection) files

extractLoops collection filename = do
  createDirectoryIfMissing False collection
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
        writeZounds sounds = mapM (contentAddressableWrite fileStub collection "wav" . writer) sounds
        fileStub = "loop-" ++ takeBaseName filename

splitIntoLoops :: Zound -> [Int] -> [Zound]
splitIntoLoops sound bars =
  map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))
