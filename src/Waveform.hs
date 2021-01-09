module Waveform
( loopToWaveform
, loopToWaveformUnsafe
) where

import Prelude hiding (concat, replicate)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.Int (Int64)
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import System.IO.Unsafe (unsafePerformIO)

import Analysis
import Loop
import Util
import Zounds

-- Efficiently generate a waveform image of a Zound. This generates it rotated
-- 90 degrees since it's simpler and un-rotating it during GPU rendering is
-- cheap (free?)

type Col = L.ByteString

fg :: Col
fg = L.pack [40, 40, 200, 200]
bg :: Col
bg = L.pack [128, 0, 128, 64]

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
zoundToWaveform w h z = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True
  where bs = L.toStrict $ generatePixels w h z

generatePixels :: Int -> Int -> Zound -> L.ByteString
generatePixels w h z = mconcat $ L.transpose rows
  where rows :: [L.ByteString]
        rows = map (generateRow fg bg (fromIntegral w)) waveWidths
        rmses :: [Float]
        rmses = rmsTo w z
        waveWidths :: [Int64]
        waveWidths = [fromIntegral $ clip 2 (h-1) $ floor $ s * ((fromIntegral h) / 2) | s <- rmses]
        --generateCol :: Float -> L.ByteString
        --generateCol s = {-eeesp ("lens", length top, length middle, length bottom) $-} top `mconcat` middle `mconcat` bottom
        --  where middle = L.replicate ht fg
        --        top = L.replicate (fromIntegral ((h - ht) `div` 2)) bg
        --        bottom = L.replicate (h - ht - length top) bg
        --        --ht = eeesp ("ht", s) $ clip 0 (h-1) $ floor $ s * fromIntegral (h `div` 2)
        --        --ht = eeesp ("ht", s) $ clip 0 (h-1) $ floor (((s * fromIntegral h) / 2) + fromIntegral (h `div` 2))
        --        -- [0,1] -> [0,h/2]
        --        ht = clip 2 (h-1) $ floor $ s * ((fromIntegral h) / 2)

-- Generate a row (actually a column of a waveform) like:
--     bbbbbbbbbbffffffffffffffffffffffbbbbbbbbbb
generateRow :: Col -> Col -> Int64 -> Int64 -> L.ByteString
generateRow fg bg totalLen middleLen = left `mappend` middle `mappend` right
  where left = L.take (leftLen * 4) (L.cycle bg)
        middle = L.take (middleLen * 4) (L.cycle fg)
        right = L.take (rightLen * 4) (L.cycle bg)
        leftLen = (totalLen - middleLen) `div` 2
        rightLen = totalLen - middleLen - leftLen

-- nOf :: Int -> a -> [a]
-- nOf n x = take n (repeat x)

clip :: (Num a, Ord a) => a -> a -> a -> a
clip low high x
  | x < low = low
  | x > high = high
  | otherwise = x
