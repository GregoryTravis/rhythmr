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

clop :: SV.Vector Float -> SV.Vector Float
clop v = SV.take num (SV.drop start v)
  where start = floor $ 0.5 * 48000 * 2
        num = floor $ 2.5 * 48000 * 2

main = do
  let file = "Grace Jones - Pull Up To The Bumper-Tc1IphRx1pk.f135.wav"
  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile file
  msp $ SV.length $ BV.fromBuffer buffer
  track <- aubioTrack file
  msp track
  --msp $ map (uncurry (flip (-))) $ zip track (tail track)
  msp info

  let clopt = clop $ BV.fromBuffer buffer
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
