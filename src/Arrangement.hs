module Arrangement
( Arrangement(..)
, Placement(..)
, Span(..)
, mapSpans
, renderArrangement
, parArrangement
, seqArrangement
, singleSoundArrangement ) where

import Control.Monad.ST
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.ST.Strict as MSV

import Resample
import Sound
import Util

-- The end sample of a Span is the sample *just after* the end of the audio
data Span = Span Int Int deriving Show
data Placement = Placement Sound Span | NRPlacement Sound Int deriving Show
data Arrangement = Arrangement [Placement] deriving Show

-- instance Functor Arrangement where
--   fmap f (Arrangement placements) = Arrangement (map f placements)

-- applyToSpan f (Placement s span) = Placement s (f span)

-- mapOverSpans :: (Spen -> Span) -> Arrangement -> Arrangement
-- mapOverSpans f arr = fmap (applyToSpan f) arr

mapOverSounds :: (Sound -> Sound) -> Arrangement -> Arrangement
mapOverSounds f (Arrangement placements) = Arrangement (map (applyToPlacementSound f) placements)
applyToPlacementSound :: (Sound -> Sound) -> Placement -> Placement
applyToPlacementSound f (Placement sound span) = Placement (f sound) span
applyToPlacementSound f (NRPlacement sound n) = NRPlacement (f sound) n

-- Convert any Placements to NRPlacements
arrPToNrp :: Arrangement -> IO Arrangement
arrPToNrp (Arrangement ps) = do
  newPs <- mapM pToNrp ps
  return $ Arrangement newPs

pToNrp :: Placement -> IO Placement
pToNrp x@(NRPlacement _ _) = return x
pToNrp (Placement sound (Span s e)) = do
  resampled <- resampleSound (e-s) sound
  return $ NRPlacement resampled s

-- All Placements must be Placements and not NRPlacements
renderArrangement :: Arrangement -> IO Sound
renderArrangement arr = do
  --msp arr
  nrpArr <- arrPToNrp arr
  --msp nrpArr
  fmap normalize $ mixNRPs (mapOverSounds normalize nrpArr)

mixNRPs :: Arrangement -> IO Sound
mixNRPs arr = do
  let len = arrangementLength arr
      mix = SV.replicate (len * 2) 0 :: SV.Vector Float
      nrps = case arr of Arrangement nrps -> nrps
      mix'' = runST $ guv mix nrps
  return Sound { samples = mix'' }
  where yeah :: MSV.Vector s Float -> Placement -> ST s ()
        yeah mix (NRPlacement sound pos) = {-eesp "mixOnto" $-} mixOnto mix (samples sound) pos
        guv :: SV.Vector Float -> [Placement] -> ST s (SV.Vector Float)
        guv mix nrps = do
          mmix <- MSV.thaw mix
          mapM (yeah mmix) nrps
          mix' <- MSV.freeze mmix
          return mix'

mixOnto :: MSV.Vector s Float -> SV.Vector Float -> Int -> ST s ()
mixOnto mix v pos = do
  mapM mixSample indices
  return ()
  where indices = take (SV.length v) [0..]
        mixSample i = do
          mixSample <- MSV.read mix (i + (pos * 2))
          let vSample = SV.index v i
          MSV.write mix (i + (pos * 2)) (mixSample + vSample)

-- This must be something
-- wha :: (a -> b -> IO a) -> a -> [b] -> IO a
-- wha f a [] = return a
-- wha f a (b : bs) = do
--   newA <- f a b
--   wha f newA bs
-- This is a fold, right?
wha :: (a -> b -> a) -> a -> [b] -> a
wha f a [] = a
wha f a (b : bs) = wha f (f a b) bs

mapPlacements :: (Placement -> Placement) -> Arrangement -> Arrangement
mapPlacements f (Arrangement ps) = Arrangement (map f ps)
mapPlacementSpan :: (Span -> Span) -> Placement -> Placement
mapPlacementSpan f (Placement sound span) = Placement sound (f span)
mapSpans :: (Span -> Span) -> Arrangement -> Arrangement
mapSpans = mapPlacements . mapPlacementSpan

arrangementLength :: Arrangement -> Int
arrangementLength (Arrangement []) = 0
arrangementLength (Arrangement nrps) = maximum (map endOf nrps)
  where endOf (NRPlacement sound start) = start + numFrames sound
        endOf (Placement sound (Span start end)) = end

singleSoundArrangement :: Int -> Sound -> Arrangement
singleSoundArrangement numFrames sound = Arrangement [Placement sound (Span 0 numFrames)]

-- Simply concatenate the lists of placements
mergeArrangements :: [Arrangement] -> Arrangement
mergeArrangements arrs = Arrangement (concat $ map getPs arrs)
  where getPs (Arrangement ps) = ps
parArrangement = mergeArrangements

seqArrangement :: [Arrangement] -> Arrangement
seqArrangement arrs = seqArrangement' 0 arrs
seqArrangement' offset (arr : arrs) =
  let len = arrangementLength arr
      tArr = translateArr offset arr
      tArrs = seqArrangement' (offset+len) arrs
   in mergeArrangements [tArr, tArrs]
seqArrangement' _ [] = Arrangement []
-- seqArrangement arrs = mergeArrangements (seqArrangements arrs)
--   where seqArrangements :: [Arrangement] -> [Arrangement]
--         seqArrangements (arr : arrs) =
--           let len = arrangementLength arr
--               translatedArrs = seqArrangements $ map (translateArr len) arrs
--            in arr : translatedArrs
--         seqArrangements [] = []

translateSpan n (Span s e) = Span (s+n) (e+n)
translateArr :: Int -> Arrangement -> Arrangement
translateArr n = mapSpans (translateSpan n)
