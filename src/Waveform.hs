module Waveform
( loopToWaveform
) where

import Data.Word
import Data.ByteString (ByteString, pack)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Loop
import State
import Util
import Zounds

loopToWaveform :: State -> Int -> Int -> Loop -> IO Picture
loopToWaveform s w h (Loop filename) = do
  let path = projectDir s ++ "/loops/" ++ filename
  msp path
  z <- readZound path
  return $ zoundToWaveform w h z

zoundToWaveform :: Int -> Int -> Zound -> Picture
zoundToWaveform w h z = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) packed True
  where packed = pack $ generatePixels w h z

generatePixels :: Int -> Int -> Zound -> [Word8]
generatePixels w h z = concat [samp x y | y <- [0..h-1], x <- [0..w-1]]
  where -- Color at a pixel
        samp x y | above = purple
                 | otherwise = redC
          where -- Position in the sound, in frames
                i = floor (fromIntegral x * ratio)
                -- Sample of the sound, [-1, 1]
                s = sampleAtFrame z i
                -- Sample of the sound, in [0, h] range
                vs = floor (((s + 1) / 2) * (fromIntegral h))
                above = y < vs
        -- Ratio of sound length to bitmap width
        ratio = fromIntegral len / fromIntegral w
        len = numFrames z

purple :: [Word8]
purple = [128, 0, 128, 64]
redC :: [Word8]
redC = [255, 0, 0, 64]

-- bitmapData :: ByteString
-- bitmapData = pack $ take 40000 (cycle purple)

-- ourPicture :: Picture
-- ourPicture = bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True
