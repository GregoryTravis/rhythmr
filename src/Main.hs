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
