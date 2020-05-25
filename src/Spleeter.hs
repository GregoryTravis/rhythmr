module Spleeter (spleeter) where

import System.Directory
import System.FilePath.Posix (takeBaseName)
import System.IO.Temp

import External
import Memoize
import Zounds
import Util

spleeter :: Zound -> IO Zound
--spleeter sound = withSystemTempDirectory "spleeter" (spleeter' sound)
--spleeter sound = spleeter' sound "/Users/gmt/rhythmr/haha"
spleeter = memoizedSpleeter

spleeter' :: Zound -> FilePath -> IO Zound
spleeter' input dir = do
  let inputFile = dir ++ "/input.wav"
  msp ("haha", inputFile)
  writeZound inputFile input
  runProc "spleeter" ["separate", "-i", inputFile, "-p", "spleeter:4stems", "-o", dir]
  let outputFile = dir ++ "/" ++ (takeBaseName inputFile) ++ "/drums.wav"
  readZound outputFile

-- Lovely ugly
memoizedSpleeter :: Zound -> IO Zound
memoizedSpleeter sound = do
  tmpDir <- getCanonicalTemporaryDirectory
  msp tmpDir
  outputDir <- createTempDirectory tmpDir "spleeter"
  outputFile <- diskMemoize "spleeter" (TakesFile (f outputDir)) sound
  spleeteredZound <- readZound outputFile
  removeDirectoryRecursive outputDir
  return spleeteredZound
  where f :: String -> String -> Zound -> IO ()
        f outputDir filename sound = withContentHashNamedFile "spleeter" writer (spleeterer outputDir filename)
        writer inputFile = writeZound inputFile sound
        spleeterer :: String -> String -> String -> IO ()
        spleeterer outputDir desiredOutputFile newInputFile = do
          runProc "spleeter" ["separate", "-i", newInputFile, "-p", "spleeter:4stems", "-o", outputDir]
          let outputFile = outputDir ++ "/" ++ (takeBaseName newInputFile) ++ "/drums.wav"
          renameFile outputFile desiredOutputFile
