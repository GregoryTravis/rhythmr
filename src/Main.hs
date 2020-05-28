module Main where

import GHC.Conc
import System.Environment (getArgs)

import Affinity
import Analysis
import Bars
import Diag
import Gfx
import Hypercube
import Looper (withPortaudio)
import Util
import Zounds

doStuffDefault = ["aff", "2345", "aaa", "2", "bbb", "1"]
--doStuffDefault = ["zound"]
--doStuffDefault = ["g"]
--doStuffDefault = ["hy"]
doStuff ["barsSearch", collection, searchString, numTracks] = barsSearch collection searchString (read numTracks)
doStuff ["barsId", collection, id] = barsId collection id
doStuff ["barsFile", collection, filename] = barsFile collection filename
doStuff ("aff" : collections) = affinityMain 2345 (parse collections)
  where parse :: [String] -> [(Double, String)]
        parse [] = []
        parse (c : w : etc) = (read w, c) : parse etc
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
