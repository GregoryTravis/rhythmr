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
import Hypercube
import Looper (withPortaudio)
import Mess
import Sound
import Util
import Zound

doStuffDefault = ["aff", "2345"]
--doStuffDefault = ["zound"]
--doStuffDefault = ["g"]
--doStuffDefault = ["hy"]
doStuff ["barsSearch", searchString, numTracks] = barsSearch searchString (read numTracks)
doStuff ["barsYouTubeURL", id] = barsYouTubeURL id
doStuff ["aff", seed] = affinityMain (read seed)
doStuff ["g"] = gfxMain
doStuff ["hy"] = hypercubeMain
doStuff ["zound"] = zoundMain
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
