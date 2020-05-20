module Spleeter (spleeter) where

import System.Directory
import System.FilePath.Posix (takeBaseName)
import System.IO.Temp

import External
import Memoize
import Sound
import Util

spleeter :: Sound -> IO Sound
--spleeter sound = withSystemTempDirectory "spleeter" (spleeter' sound)
--spleeter sound = spleeter' sound "/Users/gmt/rhythmr/haha"
spleeter = memoizedSpleeter

spleeter' :: Sound -> FilePath -> IO Sound
spleeter' input dir = do
  let inputFile = dir ++ "/input.wav"
  msp ("haha", inputFile)
  writeSound inputFile input
  runProc "spleeter" ["separate", "-i", inputFile, "-p", "spleeter:4stems", "-o", dir]
  let outputFile = dir ++ "/" ++ (takeBaseName inputFile) ++ "/drums.wav"
  readSound outputFile

-- Lovely ugly
memoizedSpleeter :: Sound -> IO Sound
memoizedSpleeter sound = do
  tmpDir <- getCanonicalTemporaryDirectory
  msp tmpDir
  outputDir <- createTempDirectory tmpDir "spleeter"
  outputFile <- diskMemoize "spleeter" (TakesFile (f outputDir)) sound
  spleeteredSound <- readSound outputFile
  removeDirectoryRecursive outputDir
  return spleeteredSound
  where f :: String -> String -> Sound -> IO ()
        f outputDir filename sound = withContentHashNamedFile "spleeter" writer (spleeterer outputDir filename)
        writer inputFile = writeSound inputFile sound
        spleeterer :: String -> String -> String -> IO ()
        spleeterer outputDir desiredOutputFile newInputFile = do
          runProc "spleeter" ["separate", "-i", newInputFile, "-p", "spleeter:4stems", "-o", outputDir]
          let outputFile = outputDir ++ "/" ++ (takeBaseName newInputFile) ++ "/drums.wav"
          renameFile outputFile desiredOutputFile
