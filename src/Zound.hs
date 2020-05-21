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

-- For the sake of simplicity (ie laziness, and not the Haskell kind), most of
-- this code treats audio as a mono sample, even though in fact it's always
-- stereo -- we convert mono to stereo when reading. In other words, we treat a
-- sample frame as a single sample, which is incorrect.
type Frame = Int

-- Bounds start end
-- end-start is the length of the sample array, not the number of sample frames
-- (which would be half the length).
data Bounds = Bounds Frame Frame

toInts :: Bounds -> [Frame]
toInts (Bounds s e) = [s..e-1]

translateBounds :: Bounds -> Frame -> Bounds
translateBounds (Bounds s e) dt = Bounds (s + dt) (e + dt)

-- inside :: Bounds -> Frame -> Bool
-- inside (Bounds s e) t = t >= s && t < e

data Processor = Processor

data Zound = Segment { samples :: SV.Vector Double, offset :: Frame }
           | Translate Frame Zound
           | Scale Double Zound
           | Affine Double Frame Zound
           | External Processor Zound
           | InternalFx (Double -> Double) Zound
           | PureFx (Frame -> Double) Bounds
           | Bounded Bounds Zound
           | Mix [Zound]

getBounds :: Zound -> Bounds
getBounds (Segment { samples, offset }) = Bounds offset (offset + SV.length samples)
getBounds (Translate dt z) = translateBounds (getBounds z) dt
getBounds (Bounded b _) = b

-- Second argument is sample #, not time; if it were time, we'd have to return
-- two samples, since it's stereo.
sample :: Zound -> Frame -> Double
sample z@(Segment { samples }) n
  | n >= 0 && n < SV.length samples = samples `SV.index` n
  | otherwise = 0
sample (Translate dt z) n = sample z (n - dt)
sample (Mix zs) n = sum (map (flip sample n) zs)
sample (Bounded _ z) n = sample z n

-- getVector :: Bounds -> SV.Vector Double  -- TODO write default definition
-- getVector = undefined

render :: Zound -> Zound
render = trivialRender

-- Just sample through the bounds
trivialRender :: Zound -> Zound
trivialRender z =
  let bounds = getBounds z
      Bounds s e = bounds
      samples = SV.pack $ map (sample z) (toInts bounds)
   in Segment { samples, offset = s }

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
  msp "start"
  let file = "loops/loop-download-6dc53e275e7b0552f632fc628de4d8b5-7738ccbb63cce757a1b2cadd823ea35c.wav"
  z <- readZound file
  let z' = Bounded (Bounds 0 800000) $ Translate (2 * 2 * 44100) z
  writeZound "foo.wav" (render z')
  msp "zhi"