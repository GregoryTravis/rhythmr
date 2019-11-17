{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
import System.IO
import System.Process

import Util

---- IO

readFromProc :: String -> [String] -> IO String
readFromProc exe args = do
  let cp = (proc exe args) { std_out = CreatePipe }
  (_, Just out, _, _) <- createProcess cp
  hGetContents out

parseAubioOutput :: String -> [[String]]
parseAubioOutput s = map words (lines s)

aubioTrack :: String -> IO [Double]
aubioTrack file = do
  s <- readFromProc "aubiotrack" [file]
  return $ map (\row -> case row of [fs] -> (read fs) :: Double) (parseAubioOutput s)

--clop :: SV.Vector Float -> SV.Vector Float
clop s e v = SV.take num (SV.drop start v)
  where start = esp $ 2 * (floor $ s * 48000)
        num = esp $ 2 * (floor $ (e - s) * 48000)

main = do
  let file = "Grace Jones - Pull Up To The Bumper-Tc1IphRx1pk.f135.wav"

  -- 120 bpm, beat = 0.5, should be 0, 0.5, 1, ...
  --let file = "clik.wav"

  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile file
  msp $ SV.length $ BV.fromBuffer buffer
  track' <- aubioTrack file
  msp $ take 10 track'
  --msp $ map (uncurry (flip (-))) $ zip track (tail track)
  msp info
  let track = dropWhile (< 40.0) track'
  msp $ take 10 track

  let clopt = clop (track !! 0) (track !! 4) $ BV.fromBuffer buffer
  msp $ SV.length clopt
  let numFrames = (SV.length clopt) `div` 2

  let cloptInfo = Info
        { frames = numFrames
        , samplerate = 48000
        , channels = 2
        , format = Format
            { headerFormat = HeaderFormatWav
            , sampleFormat = SampleFormatPcm16
            , endianFormat = EndianFile
            }
        , sections = 1
        , seekable = True
        }

  count <- SF.writeFile cloptInfo "snip.wav" (BV.toBuffer clopt)
  
  msp "hi"
