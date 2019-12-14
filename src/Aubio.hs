module Aubio
( aubioTrack
, barBeat ) where

import Sound.File.Sndfile as SF hiding (hGetContents)

import External
import Util

parseAubioOutput :: String -> [[String]]
parseAubioOutput s = map words (lines s)

aubioTrack :: String -> IO [Int]
aubioTrack file = do
  s <- readFromProc "aubiotrack" [file]
  info <- getFileInfo file
  let sampleRate = samplerate info
  let toFrame :: Double -> Int
      toFrame t = floor (t * (fromIntegral sampleRate))
  return $ map (\row -> case row of [fs] -> toFrame ((read fs) :: Double)) (parseAubioOutput s)

barBeat :: String -> IO [Int]
barBeat filename = do
  info <- getFileInfo filename
  let sampleRate = samplerate info
  let toFrame :: Double -> Int
      toFrame t = floor (t * (fromIntegral sampleRate))
  csv <- csvCommand "sonic-annotator-1.5-osx-amd64/sonic-annotator" ["-d", "vamp:qm-vamp-plugins:qm-barbeattracker:bars", filename, "-w", "csv", "--csv-stdout"]
  return $ map (toFrame . read . (!! 1)) csv
