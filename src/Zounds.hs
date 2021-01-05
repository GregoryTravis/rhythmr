{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zounds
( Zound
, ZoundT(..)
, Frame
, FSamples
, Bounds(..)
, Source(..)
, getBounds
--, getSamples
, getAllBounds
, getAllSegments
, getStart
, getEnd
, durationSeconds
, render
, renderGrid
, strictRender
, readZound
, readZoundZeroCrossings
, readZoundFadeEnds
, resample
, writeZound
, numFrames
, samplesAsFloats
, toZero
, snip
, snipBounds
, normalize
, toOrigin
, channelsToSegment
, speedTest
) where

import Control.Monad.ST
import Data.Maybe
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.ST.Strict as MSV
import Numeric.Interval
import qualified Numeric.Interval as NI
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import System.Directory
import System.IO.Temp

import BandLimitedInterpolator
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
  deriving (Eq, Show, Ord)

niceShowBounds :: Bounds -> String
niceShowBounds (Bounds s e) = "[" ++ (show s') ++ ", " ++ (show e') ++ "]"
  where s' = fromIntegral s / fromIntegral loopLengthFrames
        e' = fromIntegral e / fromIntegral loopLengthFrames

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

-- Really don't think this is right
scaleBounds :: Bounds -> Frame -> Bounds
scaleBounds (Bounds s e) frames = Bounds s (e + frames)

-- This is wrong because the semantics of Scale are wrong: Scale effectively
-- translates to the origin before, and back after; most likely, we never use
-- Scale on anything that's not at the origin. So this, too, assumes that.
scaleBoundsWrong :: Bounds -> Frame -> Bounds
scaleBoundsWrong _ n = Bounds 0 n
--scaleBoundsWrong (Bounds s e) scale = Bounds (floor $ (fromIntegral s) * scale) (floor $ (fromIntegral e) * scale)

-- inside :: Bounds -> Frame -> Bool
-- inside (Bounds s e) t = t >= s && t < e

boundsUnion :: Bounds -> Bounds -> Bounds
boundsUnion (Bounds s e) (Bounds s' e') = Bounds (min s s') (max e e')

boundsIntersection :: Bounds -> Bounds -> Maybe Bounds
boundsIntersection b0 b1 =
  let i0 = toInterval b0
      i1 = toInterval b1
      int = intersection i0 i1
   in toMaybe int
  where toMaybe int | NI.null int = Nothing
                    | otherwise = Just $ Bounds (inf int) (sup int)
        toInterval (Bounds s e) = s ... e

boundingBox :: [Bounds] -> Bounds
boundingBox [] = error "boundingBox: empty list"
boundingBox bs = foldr1 boundsUnion bs

boundsLength :: Bounds -> Frame
boundsLength (Bounds s e) = e - s

-- Represents where the sound came from, eg a filename.
data Source = Source [String]
  deriving Show

combineSources :: [Source] -> Source
combineSources sources = Source $ concat $ map unSource sources
  where unSource (Source ss) = ss

combineSourcesFrom :: [Zound] -> Source
combineSourcesFrom zs = combineSources $ catMaybes $ map getSource $ concat $ map getAllSegments zs
  where getSource (Segment { source }) = source

transferSources :: [Zound] -> Zound -> Zound
transferSources zs z@(Segment {}) = z { source }
  where source = Just $ combineSourcesFrom zs

-- Segment's offset is where the first sample in the Samples should be (not an
-- offset into the Samples)
data ZoundT a = Segment { samples :: Samples, offset :: Frame, source :: Maybe a }
           | Translate Frame Zound
           | Scale Frame Zound
           | ExternalFx Processor Zound
           | InternalFx (Int -> Double -> Double) Zound
           | MonoSynth (Frame -> Double) Bounds
           | Bounded Bounds Zound
           | Silence Bounds
           | Mix [Zound]

type Zound = ZoundT Source

-- getSamples :: Zound -> Samples
-- getSamples (Segment { samples }) = samples

instance Show (ZoundT Source) where
  show z = show' z ++ " " ++ (niceShowBounds (getBounds z))
    where show' (Segment { source }) = "[Seg " ++ (show source) ++ "]"
          show' (Translate dt z) = "(Translate " ++ (show dt) ++ " " ++ (show z) ++ ")"
          show' (Scale len z) = "(Scale " ++ (show len) ++ " " ++ (show z) ++ ")"
          show' (ExternalFx _ z) = "(ExternalFx " ++ (show z) ++ ")"
          show' (InternalFx _ z) = "(InternalFx " ++ (show z) ++ ")"
          show' (MonoSynth _ _) = "(MonoSynth)"
          show' (Bounded b z) = "(Bounded " ++ (niceShowBounds b) ++ " " ++ (show z) ++ ")"
          show' (Silence b) = "(Silence)"
          show' (Mix zs) = "(Mix " ++ show zs ++ ")"

durationSeconds :: Zound -> Double
durationSeconds z = fromIntegral (getEnd $ getBounds z) / fromIntegral standardSR

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
getBounds (Scale frames z) = scaleBounds (getBounds z) frames
getBounds (Bounded b _) = b
getBounds (Mix zs) = boundingBox (map getBounds zs)
--getBounds (ExternalFx _ z) = getBounds z
getBounds (InternalFx f z) = getBounds z
getBounds (MonoSynth f b) = b
getBounds (Silence b) = b

-- Walk the tree and get Bounds for each segment, scaling and translating as we
-- go.
getAllBounds :: Zound -> [Bounds]
getAllBounds z@(Segment {}) = [getBounds z]
getAllBounds (Translate dt z) = map (flip translateBounds dt) (getAllBounds z)
getAllBounds (Scale numFrames z) = map (flip scaleBoundsWrong numFrames) (getAllBounds z)
getAllBounds (InternalFx _ z) = getAllBounds z
getAllBounds (Bounded b z) = catMaybes $ map (boundsIntersection b) (getAllBounds z)
getAllBounds (Silence _) = []
getAllBounds (Mix zs) = concat (map getAllBounds zs)
getAllBounds z = error ("???" ++ (show z))

-- Walk the tree and get Segments.
getAllSegments :: Zound -> [Zound]
getAllSegments z@(Segment {}) = [z]
getAllSegments (Translate dt z) = getAllSegments z
getAllSegments (Scale s z) = getAllSegments z
getAllSegments (InternalFx b z) = getAllSegments z
getAllSegments (Bounded b z) = getAllSegments z
getAllSegments (Silence _) = []
getAllSegments (Mix zs) = concat (map getAllSegments zs)
getAllSegments z = error ("???" ++ (show z))

-- Second argument is sample index, not frame number.
sample :: Zound -> Int -> Double
sample (Segment { samples }) i
  | i >= 0 && i < SV.length samples = samples `SV.index` i
  | otherwise = 0
sample (Translate dt z) i = sample z (i - (dt * 2))
sample (Mix zs) i = sum (map (flip sample i) zs)
sample (Bounded _ z) i = sample z i
sample (InternalFx f z) i = f i (sample z i)
sample (MonoSynth f _) i = f (i `div` 2)
sample (Silence _) _ = 0

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
   in return $ Segment { samples, offset = s, source = Nothing }

-- Chunk up, optimize affines, etc
fastRender :: Zound -> IO Zound
fastRender z@(Segment {}) = return z
fastRender (ExternalFx p z) = do
  z' <- fastRender z
  z'' <- processZound z' p
  let source'' = combineSourcesFrom [z]
      Segment { samples = samples'', offset = offset'' } :: Zound = z''
  return $ Segment { samples = samples'', offset = offset'', source = Just $ source'' }
  --return $ z'' { source = source'' }
fastRender (Scale numFrames z) = do
  z' <- fastRender z
  resample numFrames z'
fastRender (Translate dt z) = do
  z' <- fastRender z
  return $ z' { offset = offset z' + dt }
fastRender (Mix zs) = do
  zs' <- mapM fastRender zs
  mixSegments zs'
fastRender z@(InternalFx _ _) = transferSources [z] <$> trivialRender z
fastRender z@(MonoSynth _ _) = trivialRender z
-- TODO slow
fastRender z@(Silence _) = trivialRender z
-- TODO slow, should crop the array or something
fastRender z@(Bounded _ _) = fadeEnds <$> trivialRender z


-- Split into left and right and resample separately
resample :: Int -> Zound -> IO Zound
resample numFrames z = do
  let (leftSrcSV, rightSrcSV) = segmentToChannels z
  leftDestSV <- blint numFrames leftSrcSV
  rightDestSV <- blint numFrames rightSrcSV
  return $ channelsToSegment (leftDestSV, rightDestSV) (source z)

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
  --msp ("mixSegments", length zs, allBounds)
  return $ Segment { samples = mixBuffer', offset = getStart allBounds, source = Just $ combineSourcesFrom zs }

mixOnto :: MSV.Vector s Double -> Samples -> Int -> ST s ()
mixOnto mix v offset = do
  --massert "mixOnto: length mismatch" (MSV.length mix) (SV.length v)
  mapM mixSample indices
  return ()
  where len = SV.length v
        indices = take len [0..]
        offsetSamples = offset * 2
        mixSample i = do
          mixSample <- MSV.read mix (i + offsetSamples)
          let vSample = SV.index v i
              faded = vSample * fadeEndsScaler len i
          MSV.write mix (i + offsetSamples) (mixSample + faded)

-- Return scaling factor for any point in a sample. In the middle, it's 1. At
-- the very beginning, it ramps up, and at the end, it ramps down.
-- 'fadeLength' specifies the duration of the fade.
-- Does not do bounds checking.
fadeEndsScaler :: Int -> Int -> Double
fadeEndsScaler len i
  | i < fadeLength = interp 0 (fadeLength-1) 0 1 i
  | i >= len - fadeLength = interp (len - fadeLength) (len - 1) 1 0 i
  | otherwise = 1
  where fadeLength = 220  -- 2.5ms
        -- Interpolate a value between x and y, based on where a is between i and j.
        -- Does not do bounds checking.
        interp :: Int -> Int -> Double -> Double -> Int -> Double
        interp i j x y a = ((1 - k) * x) + (k * y)
          where k = (aa - ii) / (jj - ii)
                aa = fromIntegral a
                ii = fromIntegral i
                jj = fromIntegral j

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
-- fastRender' z = do
--   msp ("fastRender'", z)
--   fastRender z

-- I still don't understand how to force thunks
strictRender :: Zound -> IO Zound
strictRender z = do
  z' <- render z
  --msp $ SV.length (samples z')
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
  let z = Segment { samples = stereoize (channels info) $ BV.fromBuffer buffer
                 , offset = 0
                 , source = Just $ Source [filename] }
  return $ normalize z
  where stereoize :: Int -> SV.Vector Float -> Samples
        stereoize 1 fs = SV.map realToFrac $ SV.interleave [fs, fs]
        stereoize 2 fs = SV.map realToFrac fs

-- Assumes 0 offset
segmentToChannels :: Zound -> (Samples, Samples)
segmentToChannels (Segment { samples, offset = 0 }) =
  let [l, r] = SV.deinterleave 2 samples
   in (l, r)

channelsToSegment :: (Samples, Samples) -> Maybe Source -> Zound
channelsToSegment (left, right) source = Segment { samples, offset = 0, source }
  where samples = SV.interleave [left, right]

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

-- resampleZoundProcessor :: Frame -> Processor
-- resampleZoundProcessor destLengthFrames z@(Segment { samples }) = soxer ["speed", show speedRatio] z
--   where speedRatio = (fromIntegral srcLengthFrames) / (fromIntegral destLengthFrames)
--         srcLengthFrames = numFrames z

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

-- This doesn't really help with popping at the ends of loops, so we use
-- fadeEnds instead.
readZoundZeroCrossings :: String -> IO Zound
readZoundZeroCrossings s = do
  z <- readZound s
  return $ clipToZeroCrossings z

fadeEnds :: Zound -> Zound
fadeEnds z = eesp ("fadeEnds", z) $ InternalFx f z
  where f i' s | i < fadeLength = s * (interp 0 (fadeLength-1) 0 1 i)
               | i >= len - fadeLength = s * (interp (len - fadeLength) (len - 1) 1 0 i)
               | otherwise = s
          where i = i' + offset z
        len = SV.length (samples z)
        fadeLength = 220  -- 2.5ms
        interp :: Int -> Int -> Double -> Double -> Int -> Double
        interp i j x y a = ((1 - k) * x) + (k * y)
          where k = (aa - ii) / (jj - ii)
                aa = fromIntegral a
                ii = fromIntegral i
                jj = fromIntegral j

readZoundFadeEnds :: String -> IO Zound
readZoundFadeEnds s = do
  z <- readZound s
  return $ fadeEnds z

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
   in mix

toZero :: Zound -> Zound
toZero z =
  let Bounds s e = getBounds z
   in Translate (-s) z

-- Chop away everything < start and >= end
-- start is the first sample, end is the sample after the last sample
snip :: Frame -> Frame -> Zound -> Zound
snip start end (Segment { samples, offset, source }) =
  let start' = start - offset
      end' = end - offset
      offset' = start'
      length = SV.length samples
      startIndex = start' * 2
      endIndex = end' * 2
      ok = startIndex < endIndex && 0 <= startIndex && endIndex <= length
      samples' = SV.take (endIndex - startIndex) (SV.drop startIndex samples)
      debug = ("snip", offset, start, end)
   in eesp debug $ Segment { samples = samples', offset = offset', source }

snipBounds :: Bounds -> Zound -> Zound
snipBounds (Bounds s e) z = {-fadeEnds $-} snip s e z

-- Translate the start to 0
toOrigin :: Zound -> Zound
toOrigin z = Translate (-s) z
  where Bounds s e = getBounds z

applyToSamples :: (Samples -> Samples) -> Zound -> Zound
applyToSamples f sound = sound { samples = f (samples sound) }

normalize :: Zound -> Zound
normalize z = applyToSamples (SV.map (/mx)) z
  where mx = max 0.005 $ SV.maximum (samples (applyToSamples (SV.map abs) z))

sineWave :: Double -> Frame -> Zound
sineWave hz len = MonoSynth f (Bounds 0 len)
  where f x = sin (fromIntegral x * k)
        k = 2 * pi * (hz / 44100.0)

-- zoundMain = do
--   msp "start"
--   -- let file = "loops/loop-download-57803dd2f53e0df8575cbcd4404b748d-2f51032daba140d5df8775f70bf232ea.wav"
--   -- z <- readZoundFadeEnds file
--   let z = sineWave 440 88200
--   let grid = [[z], [z], [z], [z]]
--       z' = renderGrid grid bpm
--   rendered <- render z'
--   writeZound "foo.wav" rendered
--   msp "zhi"

-- Add array of samples to array of samples
speedTest :: IO ()
speedTest = do
  let seconds :: Int
      nTimes :: Int
      seconds = 60
      nTimes = 5 * 30
      -- seconds = 1800
      -- nTimes = 5
      numSamples :: Int
      numSamples = 44100 * 2 * seconds
      totalSeconds :: Int
      totalSeconds = seconds * nTimes
      v0 :: SV.Vector Double
      v0 = SV.replicate numSamples 0
      v1 :: SV.Vector Double
      v1 = SV.replicate numSamples 0
      addNTimes :: Int -> SV.Vector Double -> SV.Vector Double -> ST s (SV.Vector Double)
      addNTimes numTimes v0 v1 = do
        mv0 <- MSV.thaw v0
        mapM (\_ -> mixOnto mv0 v1) (take numTimes [0..])
        MSV.freeze mv0
        --return ()
      -- addOnce :: SV.Vector Double -> SV.Vector Double -> ST s ()
      -- addOnce = do
      --   mixOnto mv0 v1
      --   MSV.freeze mv0
      mixOnto :: MSV.Vector s Double -> SV.Vector Double -> ST s ()
      mixOnto mix v = do
        --massert "mixOnto: length mismatch" (MSV.length mix) (SV.length v)
        mapM mixSample indices
        return ()
        where indices = take (SV.length v) [0..]
              mixSample i = do
                mixSample <- MSV.read mix i
                let vSample = SV.index v i
                MSV.write mix i (mixSample + vSample)
  (_, duration) <- cpuTime $ do
    let v0' :: SV.Vector Double
        v0' = runST $ addNTimes nTimes v0 v1
        aSample :: Double
        aSample = SV.index v0' 0
    msp aSample
    return $ v0'
  let secondsPerSecond = (fromIntegral totalSeconds) / duration
  msp duration
  msp $ "secondsPerSecond " ++ (show secondsPerSecond)
