module Zound
( zoundMain
) where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV

import Constants
import Util

-- readZound :: String -> IO Zound
-- readZound filename = do
--   (info, Just (buffer :: BV.Buffer Float)) <- SF.readFile filename
--   massert "sections != 1" (sections info == 1) 
--   massert ("channels: " ++ filename) (channels info == 1 || channels info == 2)
--   return $ Zound { samples = stereoize (channels info) $ BV.fromBuffer buffer }
--   where stereoize :: Int -> SV.Vector Float -> SV.Vector Float
--         stereoize 1 fs = SV.interleave [fs, fs]
--         stereoize 2 fs = fs

-- writeZound :: String -> Zound -> IO ()
-- writeZound filename sound = do
--   let numFrames = (SV.length (samples sound)) `div` 2
--   let info = Info
--         { frames = numFrames
--         , samplerate = 44100
--         , channels = 2
--         , format = Format
--             { headerFormat = HeaderFormatWav
--             , sampleFormat = SampleFormatPcm16
--             , endianFormat = EndianFile
--             }
--         , sections = 1
--         , seekable = True
--         }
--   numFramesWritten <- SF.writeFile info filename (BV.toBuffer (samples sound))
--   massert "writeZound" (numFramesWritten == numFrames)

zoundMain = do
  msp "zhi"
