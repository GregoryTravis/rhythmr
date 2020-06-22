{-# LANGUAGE NamedFieldPuns #-}

module BandLimitedInterpolator (blint) where

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector.ST.Strict as MSV
import Foreign.C.Types

import Util

-- Resample a sound to be the given length
blint :: Zound -> Int -> IO Zound
-- blint (Segment { srcSV = samples } destLen = do
--   let destSV = SV.replicate destLen 0
--       srcLen = SV.length srcSV
blint z destLen = do
  let (leftSrcSV, right srcSV) = segmentToChannels z
  leftDestSV <- blintSV (toCDoubles leftSrcSV) destLen
  rightDestSV <- blintSV (toCDoubles rightSrcSV) destLen
  return $ channelsToSegment (fromCDoubles leftDestSV) (fromCDoubles rightDestSV)

toCDoubles :: SV.Vector Double -> SV.Vector CDouble
toCDoubles = SV.map realToFrac

fromCDoubles :: SV.Vector CDouble -> SV.Vector Double
fromCDoubles = SV.map realToFrac

-- This assumes (and asserts) that the starting offset of the vector is 0,
-- which is assured above by the conversions to/from CDouble
blintSV :: SV.Vector CDouble -> Int -> IO (SV.Vector CDouble)
blintSV srcSV destLen = do
  let --destSV = SV.replicate destLen 0
      srcLen = SV.length srcSV
      (srcFP, 0, fpSrcLen) = SVB.toForeignPtr srcSV
      -- -- We don't use this ForeignPtr, we use the one we get from thaw, below
      -- (_, 0, fpDestLen) = SVB.toForeignPtr destSV
  massert "srcSV length mismatch" (srcLen == fpSrcLen)
  SVB.create destLen resample
  where resample :: Ptr CDouble -> IO ()
        resample destP = do
          withForeignPtr srcFP $ \srcP -> do
           nblint_blint srcFP srcLen destFP destLen
  --massert "destSV length mismatch" (destLen == fpDestLen)
  -- runST $ do
  --   destSVM <- MSV.thaw destSV
  --   withForeignPtr srcFP $ \srcP -> do
  --     withForeignPtr destFPM $ \destP -> do
  --       nblint_blint srcFP srcLen destFP destLe
