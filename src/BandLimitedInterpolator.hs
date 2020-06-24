{-# LANGUAGE NamedFieldPuns #-}

module BandLimitedInterpolator (blint) where

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector.ST.Strict as MSV
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Util

foreign import ccall "nblint_blint" nblint_blint :: Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO ()

toCDoubles :: SV.Vector Double -> SV.Vector CDouble
toCDoubles = SV.map realToFrac

fromCDoubles :: SV.Vector CDouble -> SV.Vector Double
fromCDoubles = SV.map realToFrac

-- Resample a sound to be the given length
blint :: Int -> SV.Vector Double -> IO (SV.Vector Double)
blint destLen srcV = do
  fromCDoubles <$> blintCD destLen (toCDoubles srcV)

-- This assumes (and asserts) that the starting offset of the vector is 0,
-- which is assured above by the conversions to/from CDouble
blintCD :: Int -> SV.Vector CDouble -> IO (SV.Vector CDouble)
blintCD destLen srcSV = do
  let --destSV = SV.replicate destLen 0
      srcLen = SV.length srcSV
      (srcFP, 0, fpSrcLen) = SVB.toForeignPtr srcSV
      -- -- We don't use this ForeignPtr, we use the one we get from thaw, below
      -- (_, 0, fpDestLen) = SVB.toForeignPtr destSV
      resample :: Ptr CDouble -> IO ()
      resample destP = do
        withForeignPtr srcFP $ \srcP -> do
          nblint_blint srcP (fromIntegral srcLen) destP (fromIntegral destLen)
  massert "srcSV length mismatch" (srcLen == fpSrcLen)
  --msp ("LENS", srcLen, destLen)
  SVB.create destLen resample
  --massert "destSV length mismatch" (destLen == fpDestLen)
  -- runST $ do
  --   destSVM <- MSV.thaw destSV
  --   withForeignPtr srcFP $ \srcP -> do
  --     withForeignPtr destFPM $ \destP -> do
  --       nblint_blint srcFP srcLen destFP destLe
