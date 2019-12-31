module Spleeter (spleeter) where

import System.FilePath.Posix (takeBaseName)

import External
import Sound
import Util

spleeter :: Sound -> IO Sound
--spleeter sound = withSystemTempDirectory "spleeter" (spleeter' sound)
spleeter sound = spleeter' sound "/Users/gmt/autobeat/haha"

spleeter' :: Sound -> FilePath -> IO Sound
spleeter' input dir = do
  let inputFile = dir ++ "/input.wav"
  msp ("haha", inputFile)
  writeSound inputFile input
  runProc "spleeter" ["separate", "-i", inputFile, "-p", "spleeter:4stems", "-o", dir]
  let outputFile = dir ++ "/" ++ (takeBaseName inputFile) ++ "/drums.wav"
  readSound outputFile
