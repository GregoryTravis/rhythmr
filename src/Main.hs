{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import GHC.Conc
import GHC.RTS.Flags
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Affinity
import Analysis
import BandLimitedInterpolator
import Bars
import Diag
import Gfx
import Hypercube
import Looper (withPortaudio)
import Project
import Util
import Zounds

helpText :: String
helpText = unlines
  [ "rhythmr command project-dir [arg, arg, arg, ...]"
  , ""
  , "Commands include:"
  , ""
  , "rhythmr barsSearch project-dir collection-name search-string num-tracks"
  , "rhythmr barsId project-dir collection id"
  , "rhythmr barsIdFile project-dir collection filename [filename, filename, ...]"
  , "rhythmr barsFile project-dir collection filename [filename, filename, ...]"
  , "rhythmr aff project-dir collection weight [collection weight, ...]" ]

doHelp :: IO ()
doHelp = putStrLn helpText

doStuff :: [String] -> IO ()
--doStuffDefault = ["zound"]
--doStuffDefault = ["g"]
--doStuffDefault = ["hy"]
doStuff ["barsSearch", projectDir, collection, searchString, numTracks] = barsSearch projectDir collection searchString (read numTracks)
doStuff ["barsId", projectDir, collection, id] = barsId projectDir collection id
doStuff ("barsIdFile" : projectDir : collection : filenames) = barsIdFile projectDir collection filenames
doStuff ("barsFile" : projectDir : collection : filenames) = barsFile projectDir collection filenames
doStuff ("aff" : projectDir : collections) = affinityMain projectDir 2345 (parse collections)
  where parse :: [String] -> [(Double, String)]
        parse [] = []
        parse (c : w : etc) = (read w, c) : parse etc
-- doStuff ["g"] = gfxMain
-- doStuff ["hy"] = hypercubeMain
-- doStuff ["zound"] = zoundMain

-- main = do
--   z <- readZound "hey.wav"
--   z' <- resample (numFrames z * 2) z
--   writeZound "hoy.wav" z'
--   z'' <- resample (numFrames z `div` 2) z
--   writeZound "hoy2.wav" z''
--   -- z'' <- readZound "hoy.wav"
--   -- msp ("finally", numFrames z, numFrames z', numFrames z'')

cleanupArgs :: [String] -> IO [String]
cleanupArgs (command : projectDir : rest) = do
  projectDir' <- initProject projectDir
  return $ command : projectDir' : rest 

-- defaultArgs :: [String] -> [String]
-- defaultArgs [] = defaultArgs'
--   where defaultArgs' = ["aff", "chitty", "chitty", "1", "jazz-drum-solo", "1"]
-- defaultArgs x = x

main :: IO ()
main = withPortaudio $ do
  noBuffering
  --getGCFlags >>= msp
  --putStrLn $ "numCapabilities: " ++ show numCapabilities
  --np <- getNumProcessors
  --putStrLn $ "getNumProcessors: " ++ show np
  args <- getArgs
  msp $ "++ rhythmr " ++ (show args)
  if null args
    then do doHelp
            exitSuccess
    else do args' <- cleanupArgs args
            doStuff args'
