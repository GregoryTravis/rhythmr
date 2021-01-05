module Waveform
( loopToWaveform
) where

import Data.Word
import Data.ByteString (ByteString, pack)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Loop
import Util
import Zounds

loopToWaveform :: Int -> Int -> Loop -> IO Picture
loopToWaveform w h (Loop filename) = do
  z <- readZound filename
  zoundToWaveform w h z

zoundToWaveform :: Int -> Int -> Zound -> IO Picture
zoundToWaveform w h z = return ourPicture

purple :: [Word8]
purple = [128, 0, 128, 64]

bitmapData :: ByteString
bitmapData = pack $ take 40000 (cycle purple)

ourPicture :: Picture
ourPicture = bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True
