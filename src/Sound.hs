{-# LANGUAGE ScopedTypeVariables #-}

module Sound where

import Data.List (nub)
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
--import System.IO
--import System.Process

import Util

data Sound = Sound { samples :: SV.Vector Float
                   , sampleRate :: Int }

applyToSamples :: (SV.Vector Float -> SV.Vector Float) -> Sound -> Sound
applyToSamples f sound = sound { samples = f (samples sound) }

applyToSamples2 :: (SV.Vector Float -> SV.Vector Float -> SV.Vector Float) -> Sound -> Sound -> Sound
applyToSamples2 f a b = Sound { samples = f (samples a) (samples b)
                              , sampleRate = sameSr }
  where sameSr = sampleRate a -- assertM (show ("sample rates differ", map sampleRate [a, b])) (sameSampleRates [a, b]) (sampleRate a)

sameSampleRates :: [Sound] -> Bool
sameSampleRates sounds = length srs == 1
  where srs = nub $ map sampleRate sounds

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
snip :: Int -> Int -> Sound -> Sound
snip start end sound = Sound { samples = newSamples, sampleRate = sr }
  where startFrame = esp $ 2 * start
        numFrames = esp $ 2 * (end - start)
        sr = sampleRate sound
        newSamples = SV.take numFrames (SV.drop startFrame (samples sound))

mixSounds :: [Sound] -> Sound
mixSounds [sound] = sound
mixSounds (sound : sounds) = mix2Sounds sound (mixSounds sounds)

mix2Sounds :: Sound -> Sound -> Sound
mix2Sounds a b = applyToSamples2 (SV.zipWith (+)) a b

appendSounds :: [Sound] -> Sound
appendSounds sounds = Sound { samples = newSamples
                            , sampleRate = newSr }
  where newSamples = SV.concat (map samples sounds)
        newSr = assertM "sample rates the same" (sameSampleRates sounds) (sampleRate (head sounds))

normalize :: Sound -> Sound
normalize sound = applyToSamples (SV.map (/mx)) sound
  where mx = SV.maximum (samples (applyToSamples (SV.map abs) sound))
