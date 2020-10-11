{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import GHC.Conc
import GHC.RTS.Flags
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Affinity
import Blossom
import Analysis
import BandLimitedInterpolator
import Bars
import Graph
import Hypercube
import Looper (withPortaudio)
import Project
import Stow
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
  , "rhythmr aff project-dir collection weight [collection weight, ...]"
  , "rhythmr demo project-dir collection weight [collection weight, ...]"
  , "rhythmr credits" ]

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
doStuff ("aff" : projectDir : collections) = affinityMain False projectDir 2345 (parseCollections collections)
doStuff ("demo" : projectDir : collections) = affinityMain True projectDir 2345 (parseCollections collections)
doStuff ["blossom", projectDir, srcCollection, destCollection, count] = blossomMain projectDir srcCollection destCollection (read count)

parseCollections :: [String] -> [(Double, String)]
parseCollections [] = []
parseCollections (c : w : etc) = (read w, c) : parseCollections etc

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

credits :: String
credits = unlines 
  [ "Thanks to:"
  , "* Icon made by Freepik from www.flaticon.com"
  , "* MacOS icon scripting from user 'valexa' (https://stackoverflow.com/a/20703594/5265393)"
  , "* MacOS app template from RichardBronosky (https://github.com/RichardBronosky/AppleScript-droplet)" ]

doCredits :: IO ()
doCredits = putStrLn credits

main :: IO ()
_main = graphTest
main = withPortaudio $ do
  noBuffering
  --getGCFlags >>= msp
  --putStrLn $ "numCapabilities: " ++ show numCapabilities
  --np <- getNumProcessors
  --putStrLn $ "getNumProcessors: " ++ show np
  args <- getArgs
  msp $ "++ rhythmr " ++ (show args)
  case args of [] -> do doHelp
                        exitSuccess
               ["credits"] -> doCredits
               args -> do args' <- cleanupArgs args
                          doStuff args'
