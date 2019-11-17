{-# LANGUAGE ScopedTypeVariables #-}

module Sound where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
--import System.IO
--import System.Process

import Util

data Sound = Sound { samples :: SV.Vector Float
                   , sampleRate :: Int }

readSound :: String -> IO Sound
readSound filename = do
  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile filename
  massert "sections != 1" (sections info == 1) 
  massert "channels != 2" (channels info == 2)
  return $ Sound { samples = BV.fromBuffer buffer
                 , sampleRate = samplerate info }

numFrames :: Sound -> Int
numFrames sound = (SV.length (samples sound)) `div` 2

writeSound :: String -> Sound -> IO ()
writeSound filename sound = do
  let numFrames = (SV.length (samples sound)) `div` 2
  let info = Info
        { frames = numFrames
        , samplerate = sampleRate sound
        , channels = 2
        , format = Format
            { headerFormat = HeaderFormatWav
            , sampleFormat = SampleFormatPcm16
            , endianFormat = EndianFile
            }
        , sections = 1
        , seekable = True
        }
  numFramesWritten <- SF.writeFile info filename (BV.toBuffer (samples sound))
  massert "writeSound" (numFramesWritten == numFrames)

--clop :: SV.Vector Float -> SV.Vector Float
-- start, end: start and end (seconds)
snip :: Double -> Double -> Sound -> Sound
snip start end sound = Sound { samples = newSamples, sampleRate = sr }
  where startFrame = esp $ 2 * (floor $ start * (fromIntegral sr))
        numFrames = esp $ 2 * (floor $ (end - start) * (fromIntegral sr))
        sr = sampleRate sound
        newSamples = SV.take numFrames (SV.drop startFrame (samples sound))
