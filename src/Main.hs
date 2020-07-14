{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import GHC.Conc
import GHC.RTS.Flags
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

doStuffDefault = ["drum-breaks-p", "aff", "drum-breaks", "1"]
--doStuffDefault = ["zound"]
--doStuffDefault = ["g"]
--doStuffDefault = ["hy"]
doStuff ["barsSearch", collection, searchString, numTracks] = barsSearch collection searchString (read numTracks)
doStuff ["barsId", collection, id] = barsId collection id
doStuff ("barsIdFile" : collection : filenames) = barsIdFile collection filenames
doStuff ("barsFile" : collection : filenames) = barsFile collection filenames
doStuff ("aff" : projectFile : collections) = affinityMain projectFile 2345 (parse collections)
  where parse :: [String] -> [(Double, String)]
        parse [] = []
        parse (c : w : etc) = (read w, c) : parse etc
doStuff ["g"] = gfxMain
doStuff ["hy"] = hypercubeMain
doStuff ["zound"] = zoundMain
doStuff [] = doStuff doStuffDefault

main = withPortaudio $ do
  noBuffering
  getGCFlags >>= msp
  putStrLn $ "numCapabilities: " ++ show numCapabilities
  np <- getNumProcessors
  putStrLn $ "getNumProcessors: " ++ show np
  args <- getArgs
  msp $ "++ " ++ (show args)
  doStuff args
  msp "hi"
