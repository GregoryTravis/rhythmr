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
import Project
import Search
import State
import Zounds
import Spleeter
import Util

doSpleeter = False

searchAndDownloadFiles :: String -> String -> Int -> IO [FilePath]
searchAndDownloadFiles collection searchString count = do
  -- The take here is because we can get more than we asked for
  ids <- fmap (take count) $ search searchString count
  msp ("ids", count, ids)
  filenames <- mapM download ids
  return filenames
  -- mapM save filenames
  -- where save filename = do
  --         msp ("copy", filename, dest filename)
  --         copyFile filename (dest filename)
  --         return $ dest filename
  --       dest filename = collection ++ "/" ++ (takeBaseName filename) ++ ".wav"

-- Download from youtube by search string; possibly remove files after
-- extracting loops.
barsSearch :: FilePath -> String -> String -> Int -> IO ()
barsSearch projectDir collection searchString numTracks = do
  filenames <- searchAndDownloadFiles collection searchString numTracks
  mapM_ (extractLoops projectDir collection) filenames

youtubeUrlPrefix = "https://www.youtube.com/watch?v="

toId :: String -> String
toId s = case stripPrefix youtubeUrlPrefix s of Just s -> s
                                                Nothing -> s

-- Download from youtube by IDs or URLs; possibly remove files after
-- extracting loops.
barsId :: FilePath -> String -> String -> IO ()
barsId projectDir collection idOrUrl = do
  --let destFilename = "tracks/" ++ id
  filename <- download (toId idOrUrl)
  --renameFile filename destFilename
  --msp destFilename
  extractLoops projectDir collection filename

-- Download from youtube by IDs or URLs listed in files; possibly remove files
-- after extracting loops.
barsIdFile :: FilePath -> String -> [String] -> IO ()
barsIdFile projectDir collection filenames = do
  fileContentses <- mapM readFile filenames
  let ids = concat (map lines fileContentses)
  mapM_ (barsId projectDir collection) ids

-- Extract loops from existing files or dirs; do not delete the files
barsFile :: FilePath -> String -> [String] -> IO ()
barsFile projectDir collection filenames = mapM_ (barsFile1 projectDir collection) filenames

-- Extract loops from existing files or dirs; do not delete the files
barsFile1 :: FilePath -> String -> String -> IO ()
barsFile1 projectDir collection filename = do
  isDir <- doesDirectoryExist filename
  files <- if isDir then getDirRecursive filename
                    else return [filename]
  mapM_ (extractLoops projectDir collection) files

extractLoops :: FilePath -> String -> FilePath -> IO ()
extractLoops projectDir collection filename = do
  dir <- getLoopDir projectDir collection
  msp filename
  bars <- barBeat filename
  original <- readZound filename
  let originalLoops = splitIntoLoops original bars
  originalFilenames <- writeZounds dir originalLoops
  msp originalFilenames
  if doSpleeter
     then do spleetered <- spleeter original
             let spleeteredLoops = splitIntoLoops spleetered bars
             spleeteredFilenames <- writeZounds dir spleeteredLoops
             msp spleeteredFilenames
     else return ()
  where writer :: Zound -> String -> IO ()
        writer sound filename = writeZound filename sound
        writeZounds :: FilePath -> [Zound] -> IO [String]
        writeZounds dir sounds = mapM (contentAddressableWrite fileStub dir "wav" . writer) sounds
        fileStub = "loop-" ++ takeBaseName filename

splitIntoLoops :: Zound -> [Int] -> [Zound]
splitIntoLoops sound bars =
  map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))
