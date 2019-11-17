{-# LANGUAGE ScopedTypeVariables #-}

module Sound where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
--import System.IO
--import System.Process

import Util

data Sound = Sound (SV.Vector Float)

readSound :: String -> IO Sound
readSound filename = do
  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile filename
  massert "sections != 1" (sections info == 1) 
  massert "channels != 2" (channels info == 2)
  return $ Sound (BV.fromBuffer buffer)

writeSound :: String -> Sound -> IO ()
writeSound filename (Sound samples) = do
  let numFrames = (SV.length samples) `div` 2
  let info = Info
        { frames = numFrames
        , samplerate = 44100
        , channels = 2
        , format = Format
            { headerFormat = HeaderFormatWav
            , sampleFormat = SampleFormatPcm16
            , endianFormat = EndianFile
            }
        , sections = 1
        , seekable = True
        }
  numFramesWritten <- SF.writeFile info filename (BV.toBuffer samples)
  massert "writeSound" (numFramesWritten == numFrames)

--clop :: SV.Vector Float -> SV.Vector Float
-- start, end: start and end (seconds)
snip :: Double -> Double -> Sound -> Sound
snip start end (Sound samples) = Sound $ SV.take numFrames (SV.drop startFrame samples)
  where startFrame = esp $ 2 * (floor $ start * 44100)
        numFrames = esp $ 2 * (floor $ (end - start) * 44100)
