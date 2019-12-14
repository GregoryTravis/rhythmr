{-# LANGUAGE ScopedTypeVariables #-}

module Sound where

import Data.List (nub)
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
--import System.IO
--import System.Process

import Util

data Sound = Sound { samples :: SV.Vector Float }

instance Show Sound where show sound = "Sound[" ++ (show (SV.length (samples sound))) ++ "]"

applyToSamples :: (SV.Vector Float -> SV.Vector Float) -> Sound -> Sound
applyToSamples f sound = sound { samples = f (samples sound) }

applyToSamples2 :: (SV.Vector Float -> SV.Vector Float -> SV.Vector Float) -> Sound -> Sound -> Sound
applyToSamples2 f a b = Sound { samples = f (samples a) (samples b) }

readSound :: String -> IO Sound
readSound filename = do
  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile filename
  massert "sections != 1" (sections info == 1) 
  massert ("channels != 2: " ++ filename) (channels info == 2)
  return $ Sound { samples = BV.fromBuffer buffer }

numFrames :: Sound -> Int
numFrames sound = (SV.length (samples sound)) `div` 2

writeSound :: String -> Sound -> IO ()
writeSound filename sound = do
  let numFrames = (SV.length (samples sound)) `div` 2
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
  numFramesWritten <- SF.writeFile info filename (BV.toBuffer (samples sound))
  massert "writeSound" (numFramesWritten == numFrames)

--clop :: SV.Vector Float -> SV.Vector Float
-- start, end: start and end (seconds)
snip :: Int -> Int -> Sound -> Sound
snip start end sound = Sound { samples = newSamples }
  where startFrame = esp $ 2 * start
        numFrames = esp $ 2 * (end - start)
        newSamples = SV.take numFrames (SV.drop startFrame (samples sound))

mixSounds :: [Sound] -> Sound
mixSounds [sound] = sound
mixSounds (sound : sounds) = mix2Sounds sound (mixSounds sounds)

mix2Sounds :: Sound -> Sound -> Sound
mix2Sounds a b = applyToSamples2 (SV.zipWith (+)) a b

appendSounds :: [Sound] -> Sound
appendSounds sounds = Sound { samples = newSamples }
  where newSamples = SV.concat (map samples sounds)

normalize :: Sound -> Sound
normalize sound = applyToSamples (SV.map (/mx)) sound
  where mx = SV.maximum (samples (applyToSamples (SV.map abs) sound))

rangeSeconds filename start end = do
  info <- getFileInfo filename
  let sr = fromIntegral (samplerate info)
  return (s/sr, e/sr)
  where s = fromIntegral start
        e = fromIntegral end
