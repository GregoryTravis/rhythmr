module Main where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
import System.IO
import System.Process

import Aubio
import Resample
import Sound
import Util

bmp = 120
meter = 4
loopLengthSeconds = (60.0 / fromInteger bmp) * meter
standardSR = 44100
loopLengthFrames = floor $ (fromInteger standardSR) * loopLengthSeconds

main = do
  let file = "Grace Jones - Pull Up To The Bumper-Tc1IphRx1pk.f135.wav"
  sound <- readSound file
  track' <- aubioTrack file
  let track = dropWhile (< 40.0) track'
  let loop = snip (track !! 0) (track !! 4) sound
  writeSound "snip.wav" loop
  msp $ numFrames loop
  loop' <- resampleSound (numFrames loop * 2) loop
  writeSound "snap.wav" loop'
  msp "hi"
