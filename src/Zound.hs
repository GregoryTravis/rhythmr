{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zound
( Zound(..)
, FSamples
, render
, renderGrid
, strictRender
, zoundMain
, readZound
, readZoundZeroCrossings
, writeZound
, numFrames
, samplesAsFloats
, snip
, normalize
) where

import Control.Monad.ST
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.ST.Strict as MSV
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import System.Directory
import System.IO.Temp

import Constants
import External
import Util

-- A Zound is a piece of sound.
--
-- More precisely, it is like a function from time (in sample frames) to amplitude
-- It also defines start and end bounds, which include all nonzero samples,
-- pluz the first zero sample after the nonzero samples (ie, the usual array
-- bounds thing).
--
-- Every sound is stereo. Mono sounds are converted to stereo, and anything
-- else is an error. (TODO)
--
-- Time is measured in frames. The length of the sample buffer is twice the
-- length of the sound in frames.
type Frame = Int

type Samples = SV.Vector Double
type FSamples = SV.Vector Float

-- Bounds start end
-- start inclusive, end exclusive, of course.
data Bounds = Bounds Frame Frame
  deriving (Eq, Show)

-- inBounds :: Bounds -> Frame -> Bool
-- inBounds (Bounds s e) t = s <= t && t < e

-- Return a sequence of sample indices for the given frame range.
-- Sample indicies for sample n are 2*n and 2*n+1
toSampleIndices :: Bounds -> [Frame]
toSampleIndices (Bounds s e) = [s'..e'-1]
  where s' = s * 2
        e' = e * 2

getStart (Bounds s e) = s
getEnd (Bounds s e) = e

translateBounds :: Bounds -> Frame -> Bounds
translateBounds (Bounds s e) dt = Bounds (s + dt) (e + dt)

-- inside :: Bounds -> Frame -> Bool
-- inside (Bounds s e) t = t >= s && t < e

boundsUnion :: Bounds -> Bounds -> Bounds
boundsUnion (Bounds s e) (Bounds s' e') = Bounds (min s s') (max e e')

boundingBox :: [Bounds] -> Bounds
boundingBox [] = error "boundingBox: empty list"
boundingBox bs = foldr1 boundsUnion bs

boundsLength :: Bounds -> Frame
boundsLength (Bounds s e) = e - s

data Zound = Segment { samples :: Samples, offset :: Frame }
           | Translate Frame Zound
           | Scale Frame Zound
           | Affine Double Frame Zound
           | ExternalFx Processor Zound
           | InternalFx (Double -> Double) Zound
           | PureFx (Frame -> Double) Bounds
           | Bounded Bounds Zound
           | Mix [Zound]

instance Show Zound where
  show z@(Segment {}) = "[Segment " ++ show (getBounds z, offset z) ++ "]"
  show (Translate _ _) = "Translate"
  show (Scale _ _) = "Scale"
  show (Affine _ _ _) = "Affine"
  show (ExternalFx _ _) = "ExternalFx"
  show (InternalFx _ _) = "InternalFx"
  show (PureFx _ _) = "PureFx"
  show (Bounded _ _) = "Bounded"
  show (Mix _ ) = "Mix"

samplesAsFloats :: Zound -> FSamples
samplesAsFloats = (SV.map realToFrac) . samples

isSegment :: Zound -> Bool
isSegment (Segment {}) = True
isSegment _ = False

numFrames :: Zound -> Int
numFrames (Segment { samples }) = SV.length samples `div` 2

getBounds :: Zound -> Bounds
getBounds z@(Segment { samples, offset }) = Bounds offset (offset + numFrames z)
getBounds (Translate dt z) = translateBounds (getBounds z) dt
getBounds (Bounded b _) = b
getBounds (Mix zs) = boundingBox (map getBounds zs)
--getBounds (ExternalFx _ z) = getBounds z

-- Second argument is sample index, not frame number.
sample :: Zound -> Int -> Double
sample (Segment { samples }) i
  | i >= 0 && i < SV.length samples = samples `SV.index` i
  | otherwise = 0
sample (Translate dt z) i = sample z (i - (dt * 2))
sample (Mix zs) i = sum (map (flip sample i) zs)
sample (Bounded _ z) i = sample z i

-- getVector :: Bounds -> Samples  -- TODO write default definition
-- getVector = undefined

-- Given input and output wav files, return an exec-able command + arg list
type Processor = Zound -> String -> String -> [String]

-- Just sample through the bounds
-- Not implemented for ExternalFx
-- Doesn't really need to be in IO but it is so it matches fastRender
trivialRender :: Zound -> IO Zound
trivialRender z =
  let bounds = getBounds z
      Bounds s e = bounds
      samples = SV.pack $ map (sample z) (toSampleIndices bounds)
   in return $ Segment { samples, offset = s }

-- Chunk up, optimize affines, etc
fastRender :: Zound -> IO Zound
fastRender z@(Segment _ _) = return z
-- External is now hard-coded to reverb
fastRender (ExternalFx p z) = do
  z' <- fastRender z
  processZound z' p
fastRender (Scale numFrames z) = fastRender (ExternalFx (resampleZound numFrames) z)
fastRender (Translate dt z) = do
  z' <- fastRender z
  return $ z' { offset = offset z' + dt }
fastRender (Mix zs) = do
  zs' <- mapM fastRender zs
  mixSegments zs'

-- mixNRPs :: Arrangement -> IO Zound
-- mixNRPs arr = do
--   let len = arrangementLength arr
--       mix = SV.replicate (len * 2) 0 :: Samples
--       nrps = case arr of Arrangement nrps -> nrps
--       mix'' = runST $ guv mix nrps
--   return Zound { samples = mix'' }
--   where yeah :: MSV.Vector s Double -> Placement -> ST s ()
--         yeah mix (NRPlacement sound pos) = {-eesp "mixOnto" $-} mixOnto mix (samples sound) pos
--         guv :: Samples -> [Placement] -> ST s (Samples)
--         guv mix nrps = do
--           mmix <- MSV.thaw mix
--           mapM (yeah mmix) nrps
--           mix' <- MSV.freeze mmix
--           return mix'

mixSegments :: [Zound] -> IO Zound
mixSegments [z] = return z
mixSegments [] = error "mixSegments: empty list"
mixSegments zs = do
  massert "mixSegments: not a segment" (all isSegment zs)
  let allBounds = boundingBox (map getBounds zs)
      --mixLength = boundsLength allBounds
      mixLength = getEnd allBounds
      mixBuffer = SV.replicate (mixLength * 2) 0 :: Samples
      mixBuffer' = runST addAll
      addAll = do
        mutableMixBuffer <- MSV.thaw mixBuffer
        mapM (mixSegmentOnto mutableMixBuffer) zs
        MSV.freeze mutableMixBuffer
      mixSegmentOnto mutableMixBuffer (Segment { samples, offset }) = do
        mixOnto mutableMixBuffer samples offset
  msp ("mixSegments", length zs, allBounds)
  return $ Segment { samples = mixBuffer', offset = getStart allBounds }

mixOnto :: MSV.Vector s Double -> Samples -> Int -> ST s ()
mixOnto mix v offset = do
  --massert "mixOnto: length mismatch" (MSV.length mix) (SV.length v)
  mapM mixSample indices
  return ()
  where indices = take (SV.length v) [0..]
        offsetSamples = offset * 2
        mixSample i = do
          mixSample <- MSV.read mix (i + offsetSamples)
          let vSample = SV.index v i
          MSV.write mix (i + offsetSamples) (mixSample + vSample)

processZound :: Zound -> Processor -> IO Zound
processZound z processor = runViaFilesCmd "wav" writeZound readZound (processor z) z

-- soxZound :: Zound -> [String] -> Zound
-- soxZound z args = processZound cmd
--   where cmd s d = ["sox", "-G", s, d] ++ args
soxer :: [String] -> Processor
soxer soxArgs _ s d = ["sox", "-G", s, d] ++ soxArgs

render :: Zound -> IO Zound
--render = trivialRender
render = fastRender

-- I still don't understand how to force thunks
strictRender :: Zound -> IO Zound
strictRender z = do
  z' <- render z
  msp $ SV.length (samples z')
  return z'

-- -- Factor out with external fx?
-- resampleZound :: Int -> Zound -> IO Zound
-- resampleZound destLengthFrames z = do
--   runViaFilesCmd "wav" writeZound readZound resample z
--   where resample src dest = ["sox", "-G", src, dest, "speed", show speedRatio]
--         speedRatio = (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
--         srcLengthFrames = numFrames sound

readZound :: FilePath -> IO Zound
readZound filename = do
  (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile filename
  massert "sections != 1" (sections info == 1) 
  massert ("channels: " ++ filename) (channels info == 1 || channels info == 2)
  return $ Segment { samples = stereoize (channels info) $ BV.fromBuffer buffer
                   , offset = 0 }
  where stereoize :: Int -> SV.Vector Float -> Samples
        stereoize 1 fs = SV.map realToFrac $ SV.interleave [fs, fs]
        stereoize 2 fs = SV.map realToFrac fs

writeZound :: String -> Zound -> IO ()
writeZound filename z = do
  let Segment { samples } = normalize z
  let info = Info
        { frames = numFrames z
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
  massert "writeZound" (numFramesWritten == numFrames z)
-- writeZound filename z = do
--   z' <- render z
--   writeZound filename z'

resampleZound :: Int -> Processor
resampleZound destLengthFrames z@(Segment { samples }) = soxer ["speed", show speedRatio] z
  where speedRatio = (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
        srcLengthFrames = numFrames z

-- Find the first and last zero-crossing and clip to those.
-- If they aren't very close to the end, we print a warning
clipToZeroCrossings :: Zound -> Zound
clipToZeroCrossings z = check z $ z { samples = samples' }
  where samples' = ensureEven $ SV.reverse (clipFront (SV.reverse (clipFront (samples z))))
        -- Print a warning if we removed a substantial amount
        check before after =
          let blen = SV.length (samples before)
              alen = SV.length (samples after)
              warning = "WARNING zc clip loss " ++ (show blen) ++ " " ++ (show alen)
           in if (blen - alen) >= tooMuch
                 then eesp warning after
                 else eesp (blen-alen, blen, alen) after
        ensureEven :: Samples -> Samples
        ensureEven ss | isEven $ SV.length ss = ss
                      | otherwise = SV.tail ss
        isEven x = (x `mod` 2) == 0
tooMuch = 50 :: Int

clipFront :: Samples -> Samples
clipFront ss = do
  let startSign = sign $ SV.head ss
      sign x = x > 0
   in SV.dropWhile ((== startSign) . sign) ss 

readZoundZeroCrossings :: String -> IO Zound
readZoundZeroCrossings s = do
  z <- readZound s
  return $ clipToZeroCrossings z

-- Lay out a sequence of stacks, resampled to the given bpm. Does not render.
renderGrid :: [[Zound]] -> Int -> Zound
renderGrid zses bpm =
  let numFrames = toLoopLengthFrames bpm
      zses' = placeMeasuresInTime (sameLength zses)
      sameLength zses = map (map toLength) zses
      toLength z = Scale numFrames z
      placeMeasuresInTime zses = zipWith moveToMeasure zses [0..]
      moveToMeasure zs n = map (Translate (n * numFrames)) zs
      mix = Mix (concat zses')
   in eesp ("ha", numFrames) $ mix

-- start is the first sample, end is the sample after the last sample
snip :: Frame -> Frame -> Zound -> Zound
snip start end (Segment { samples, offset }) =
  let start' = start - offset
      end' = end - offset
      offset' = start'
      length = SV.length samples
      startIndex = start' * 2
      endIndex = end' * 2
      ok = startIndex < endIndex && 0 <= startIndex && endIndex <= length
      samples' = SV.take (endIndex - startIndex) (SV.drop startIndex samples)
   in Segment { samples = samples', offset = offset' }

applyToSamples :: (Samples -> Samples) -> Zound -> Zound
applyToSamples f sound = sound { samples = f (samples sound) }

normalize :: Zound -> Zound
normalize z = applyToSamples (SV.map (/mx)) z
  where mx = max 0.005 $ SV.maximum (samples (applyToSamples (SV.map abs) z))

zoundMain = do
  msp "start"
  let file = "orig-loops/loop-09de23642a52b0ec4f7d5a655cd55421.wav"
      file2 = "orig-loops/loop-0d9b22ad63a501dfdb123b3c9f6e36bf.wav"
      reverb = soxer ["reverb", "85"]
      reverser = soxer ["reverse"]
      resampler = resampleZound (1 * 44100)
  z <- readZound file
  z2 <- readZound file2
  -- let z' = Bounded (Bounds 0 800000) $ Translate (2 * 2 * 44100) z
  -- let z' = ExternalFx resampler z
  -- let z' = Scale (4 * 44100) z
  -- let z' = Translate 44100 $ ExternalFx reverb $ Scale (4 * 44100) z
  -- let z' = Mix [z, Translate 44100 $ ExternalFx reverb $ Scale (4 * 44100) z]
  -- let z' = Mix [z, z, z]
  let -- grid = [[z], [z2], [z], [z2]]
      rz = ExternalFx reverser z
      rz2 = ExternalFx reverser z2
      grid = [[z], [z2], [rz], [rz2], [z, rz], [z2, rz2], [z, rz2], [rz, z2]]
      z' = renderGrid grid 120
  rendered <- render z'
  --msp rendered
  writeZound "foo.wav" rendered
  msp "zhi"
