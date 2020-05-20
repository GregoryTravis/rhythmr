{-# LANGUAGE ExistentialQuantification #-}

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

{-
data Bounds = Bounds Int Int

class Zound a where
  getBounds :: a -> Bounds
  sample :: t -> Double
  getVector :: Bounds -> SV.Vector Double  -- TODO write default definition

data Segment = Segment { samples :: SV.Vector Double, offset :: Int }
instance Zound Segment where
  getBounds (Segment { samples, offset }) = Bounds offset (offset + SV.length samples)

data Translation = Translation Int Zound

data Stretch = Stretch Double Zound

data Affine = Affine Double Int Zound

data External = External Processor Zound

data Internal = Internal (Double -> Double) Zound

data Pure = Pure (Int -> Double) Bounds

data Mix = forall a. Zound a => Mix [zound]

-- Assert different bounds; this is complex to implement for every single type
-- of sub-zound, so just do a fast version for use by renderMix, and for
-- everything else, do the slow sampling way, and optimize as needed
data Bounded = Bounded Bounds Zound

zoundToSegment :: Zound a => a -> Segment

-- Output Zound always starts at 0
renderMix :: Mix -> Zound
renderMix = trivialRenderMz

-- Just sample through the bounds
trivialRenderMix :: Mix -> Zound

-- Chunk up, optimize affines, etc
fastRenderMix :: Mix -> Zound
fastRenderMix = undefined

-- readZound 
-- writeZound
-}

zoundMain = do
  msp "zhi"
