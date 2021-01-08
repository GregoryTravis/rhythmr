module Waveform
( loopToWaveform
, loopToWaveformUnsafe
) where

import Data.ByteString (ByteString, pack)
import Data.List (transpose)
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import System.IO.Unsafe (unsafePerformIO)

import Analysis
import Loop
import Util
import Zounds

fg :: [Word8]
fg = [40, 40, 200, 200]
bg :: [Word8]
bg = [128, 0, 128, 64]

-- I am deeply ashamed; but all the files are already there and don't move or
-- change, so it should work.
loopToWaveformUnsafe :: FilePath -> Int -> Int -> Loop -> Picture
loopToWaveformUnsafe projectDir w h loop = unsafePerformIO $ loopToWaveform projectDir w h loop 

loopToWaveform :: FilePath -> Int -> Int -> Loop -> IO Picture
loopToWaveform projectDir w h (Loop filename) = do
  let path = projectDir ++ "/loops/" ++ filename
  msp path
  z <- readZound path
  return $ zoundToWaveform w h (normalize z)

zoundToWaveform :: Int -> Int -> Zound -> Picture
zoundToWaveform w h z = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) packed True
  where packed = pack $ generatePixels w h z

generatePixels :: Int -> Int -> Zound -> [Word8]
generatePixels w h z = concat $ concat $ {-eeesp "columns" $-} transpose columns
  where columns = map generateCol rmses
        rmses = rmsTo w z
        generateCol :: Float -> [[Word8]]
        generateCol s = {-eeesp ("lens", length top, length middle, length bottom) $-} top ++ middle ++ bottom
          where middle = nOf ht fg
                top = nOf ((h - ht) `div` 2) bg
                bottom = nOf (h - ht - length top) bg
                --ht = eeesp ("ht", s) $ clip 0 (h-1) $ floor $ s * fromIntegral (h `div` 2)
                --ht = eeesp ("ht", s) $ clip 0 (h-1) $ floor (((s * fromIntegral h) / 2) + fromIntegral (h `div` 2))
                -- [0,1] -> [0,h/2]
                ht = clip 2 (h-1) $ floor $ s * ((fromIntegral h) / 2)

nOf :: Int -> a -> [a]
nOf n x = take n (repeat x)

clip :: (Num a, Ord a) => a -> a -> a -> a
clip low high x
  | x < low = low
  | x > high = high
  | otherwise = x
