module Main where

import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Sound.File.Sndfile as SF hiding (hGetContents)
import qualified Data.StorableVector as SV
import System.IO
import System.Process

import Aubio
import Resample
import Sequence
import Sound
import Util

toSequence :: [[Int]] -> Sequence Int
toSequence nses = Seq (map (\ns -> Par (map Elem ns)) nses)
--theSequence = Seq [Par [Elem 1], Par [Elem 1, Elem 2]]
theSequence = toSequence [[1], [1, 2]]

main = do
  let file = "Grace Jones - Pull Up To The Bumper-Tc1IphRx1pk.f135.wav"
  renderSequence theSequence [file]
  msp "hi"
