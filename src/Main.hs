module Main where

import GHC.Conc
import System.Environment (getArgs)

import Affinity
import Analysis
import Arrangement
import Bars
import Diag
import Feh
import Gfx
import Looper (withPortaudio)
import Mess
import Sound
import Util

--doStuffDefault = ["aff", "2345"]
doStuffDefault = ["g"]
doStuff ["bars", searchString, numTracks] = bars searchString (read numTracks)
doStuff ["aff", seed] = affinityMain (read seed)
doStuff ["g"] = gfxMain
doStuff [] = doStuff doStuffDefault

main = withPortaudio $ do
  noBuffering
  putStrLn $ "numCapabilities: " ++ show numCapabilities
  np <- getNumProcessors
  putStrLn $ "getNumProcessors: " ++ show np
  args <- getArgs
  msp $ "++ " ++ (show args)
  doStuff args
  msp "hi"
