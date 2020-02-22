module Main where

--import Data.List.Utils (replace)
--import System.Directory
import System.Environment (getArgs)

import Affinity
import Analysis
import Arrangement
import Bars
import Feh
import Looper (withPortaudio)
import Mess
import RPC
import Sound
import Terminal
import TUI
import Util

_main = do
  s0 <- readSound "loop-0-20732.wav"
  s1 <- readSound "loop-1-20732.wav"
  -- msp $ SV.length (samples s0)
  -- let arr = Arrangement [Placement s0 (Span 0 b), Placement s1 (Span b (b*2))]
  --     b = 44100 * 2
  let arr = scrunch 16 0.75 s0
      arr2 = scrunch 16 0.75 s1
      arr' = seqArrangement [arr, arr2, arr, arr2]
  song <- renderArrangement arr'
  writeSound "hoho.wav" song
  msp "hi"

fehmain = do
  s0 <- readSound "loop-0-20732.wav"
  let s1 = render s0
  writeSound "hehe.wav" s1

doStuffDefault = ["aff", "2345"]
doStuff ["bars", searchString, numTracks] = bars searchString (read numTracks)
--doStuff ["song", seed] = song (read seed)
doStuff ["aff", seed] = affinityMain (read seed)
doStuff ["rpc"] = rpc
doStuff ["displayServer"] = displayServer
doStuff ["dm"] = displayMain
doStuff ["displaySend"] = withTerminal $ \d -> do
  displaySend d (Circ 70)
  return ()
doStuff [] = doStuff doStuffDefault

main = withPortaudio $ do
  noBuffering
  args <- getArgs
  msp $ "++ " ++ (show args)
  doStuff args
  msp "hi"
