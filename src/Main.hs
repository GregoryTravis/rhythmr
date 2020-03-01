module Main where

--import Data.List.Utils (replace)
--import System.Directory
import System.Environment (getArgs)

import Affinity
import Analysis
import Arrangement
import Bars
import Diag
import Feh
import Looper (withPortaudio)
import Mess
import Sound
import TUI
import Util

doStuffDefault = ["aff", "2345"]
doStuff ["bars", searchString, numTracks] = bars searchString (read numTracks)
doStuff ["aff", seed] = affinityMain (read seed)
doStuff [] = doStuff doStuffDefault

--main = diagMain

main = withPortaudio $ do
  noBuffering
  args <- getArgs
  msp $ "++ " ++ (show args)
  doStuff args
  msp "hi"
