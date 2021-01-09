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

-- fg :: Col
-- fg = L.pack [40, 40, 200, 200]
bg :: Col
bg = L.pack [128, 0, 128, 64]

-- I am deeply ashamed; but all the files are already there and don't move or
-- change, so it should work.
loopToWaveformUnsafe :: FilePath -> Int -> Int -> Loop -> Color -> Picture
loopToWaveformUnsafe projectDir w h loop color = unsafePerformIO $ loopToWaveform projectDir w h loop color

loopToWaveform :: FilePath -> Int -> Int -> Loop -> Color -> IO Picture
loopToWaveform projectDir w h (Loop filename) color = do
  let path = projectDir ++ "/loops/" ++ filename
  msp path
  z <- readZound path
  return $ zoundToWaveform w h (normalize z) color

zoundToWaveform :: Int -> Int -> Zound -> Color -> Picture
zoundToWaveform w h z color = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True
  where bs = L.toStrict $ generatePixels w h z colorBS
        colorBS = colorToBS color

colorToBS :: Color -> L.ByteString
colorToBS color =
  let (r, g, b, a) = rgbaOfColor color
      rgbaW8 = map toW8 [r, g, b, a]
   in L.pack rgbaW8
  where toW8 :: Float -> Word8
        toW8 x = fromIntegral $ floor (x * 255)

generatePixels :: Int -> Int -> Zound -> L.ByteString -> L.ByteString
generatePixels w h z fg = toLazyByteString $ mconcat rows
  where rows :: [Builder]
        rows = map (generateRow fg bg w) waveWidths
        rmses :: [Float]
        rmses = rmsTo w z
        waveWidths :: [Int]
        waveWidths = [clip 2 (h-1) $ floor $ 4 * s * ((fromIntegral h) / 2) | s <- rmses]
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
generateRow :: Col -> Col -> Int -> Int -> Builder
generateRow fg bg totalLen middleLen =
  let fgB = lazyByteString fg
      bgB = lazyByteString bg
      left = strip leftLen bgB
      middle = strip middleLen fgB
      right = strip rightLen bgB
      leftLen = (totalLen - middleLen) `div` 2
      rightLen = totalLen - middleLen - leftLen
      strip :: Int -> Builder -> Builder
      strip n builder = mconcat (take n' (repeat builder))
        where n' = fromIntegral n
  in left `mappend` middle `mappend` right

-- nOf :: Int -> a -> [a]
-- nOf n x = take n (repeat x)

clip :: (Num a, Ord a) => a -> a -> a -> a
clip low high x
  | x < low = low
  | x > high = high
  | otherwise = x
