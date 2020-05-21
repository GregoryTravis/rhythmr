{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zound
( zoundMain
) where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV

import Constants
import Util

-- A Zound is a piece of sound.
--
-- More precisely, it is like a function from time (in sample frames) to amplitude
-- It also defines start and end bounds, which include all nonzero samples,
-- pluz the first zero sample after the nonzero samples (ie, the usual array
-- bounds thing).
--
-- Types of Zounds:
--   vector of doubles
--   translation
--   stretch
--   external fx
--   internal fx (D -> D)
--
-- Minimal complete definitino: getBounds, sample

-- A frame is a unit of time, one sample. Since everything is stereo,
-- one frame corresponds to two samples. TODO: make not everything stereo.
type Frame = Int

data Bounds = Bounds Frame Frame

-- toInts :: Bounds -> [Int]
-- toInts (Bounds s e) = [s..e-1]

data Processor = Processor

data Zound = Segment { samples :: SV.Vector Double, offset :: Frame }
           | Translation Frame Zound
           | Stretch Double Zound
           | Affine Double Frame Zound
           | External Processor Zound
           | InternalFx (Double -> Double) Zound
           | PureFx (Frame -> Double) Bounds
           | Mix [Zound]

getBounds :: Zound -> Bounds
getBounds (Segment { samples, offset }) = Bounds offset (offset + SV.length samples)

-- Second argument is sample #, not time; if it were time, we'd have to return
-- two samples, since it's stereo.
sample :: Zound -> Int -> Double
sample z@(Segment { samples }) n
  | n >= 0 && n < SV.length samples = samples `SV.index` n
  | otherwise = 0

-- getVector :: Bounds -> SV.Vector Double  -- TODO write default definition
-- getVector = undefined

render :: Zound -> Zound
render = trivialRender

-- Just sample through the bounds
trivialRender :: Zound -> Zound
trivialRender z@(Segment { samples }) = Segment { samples = samples', offset = 0 }
  where samples' = SV.pack $ map (sample z) [0..SV.length samples - 1]

-- Chunk up, optimize affines, etc
fastRenderMix :: Zound -> Zound
fastRenderMix = undefined

readZound :: FilePath -> IO Zound
readZound filename = do
  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile filename
  massert "sections != 1" (sections info == 1) 
  massert ("channels: " ++ filename) (channels info == 1 || channels info == 2)
  return $ Segment { samples = stereoize (channels info) $ BV.fromBuffer buffer
                   , offset = 0 }
  where stereoize :: Int -> SV.Vector Float -> SV.Vector Double
        stereoize 1 fs = SV.map realToFrac $ SV.interleave [fs, fs]
        stereoize 2 fs = SV.map realToFrac fs

writeZound :: String -> Zound -> IO ()
writeZound filename (Segment { samples }) = do
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
  massert "writeZound" (numFramesWritten == numFrames)

zoundMain = do
  let file = "loops/loop-download-6dc53e275e7b0552f632fc628de4d8b5-7738ccbb63cce757a1b2cadd823ea35c.wav"
  z <- readZound file
  let z' = render z
  writeZound "foo.wav" z'
  msp "zhi"
