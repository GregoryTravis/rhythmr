module Main where

--import Data.List.Utils (replace)
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)

import Affinity
import Analysis
import Arrangement
import Aubio
import Terminal
import Download
import External (contentAddressableWrite)
import Feh
import Gui
import Looper (withPortaudio)
import Mess
import RPC
import Search
import Song
import Sound
import Spleeter
import TUI
import Util

theArrayArrangement =
  [ [0]
  , [0, 1]
  , [0, 2]
  , [0, 3] -- m 6
  , [0, 1, 2, 3]
  , [0, 1, 2, 3]
  , [4]
  , [4, 5] -- m 14
  , [4, 5, 0]
  , [4, 5, 2]
  , [2, 3, 4, 6] -- m
  , [0, 1, 2, 3, 4, 5, 6] -- m
  , [0, 1, 2, 3, 4, 5, 6] ] -- m

showit vs = mapM putStrLn (map s vs)
  where s ((t0, t1), dx) = (show t0) ++ " " ++ (show dx)

downloadMain searchString count = do
  ids <- search searchString count
  msp "ids"
  msp ids
  filenames <- mapM download ids
  mapM save filenames
  where save filename = do
          msp ("copy", filename, dest filename)
          createDirectoryIfMissing True dir
          copyFile filename (dest filename)
          return $ dest filename
        dir = "tracks/" ++ searchStringDir
        dest filename = dir ++ "/" ++ (takeBaseName filename) ++ ".wav"
        searchStringDir = replace ' ' '-' searchString

bars :: String -> Int -> IO ()
bars searchString numTracks = do
  filenames <- downloadMain searchString 8
  --filenames <- downloadMain "grace jones" 2
  mapM_ extractLoops filenames

extractLoops filename = do
  msp filename
  bars <- fmap (take 8 . drop 10) $ barBeat filename
  original <- readSound filename
  let originalLoops = splitIntoLoops original bars
  originalFilenames <- writeSounds originalLoops
  msp originalFilenames
  spleetered <- spleeter original
  let spleeteredLoops = splitIntoLoops spleetered bars
  spleeteredFilenames <- writeSounds spleeteredLoops
  msp spleeteredFilenames
  where writer :: Sound -> String -> IO ()
        writer sound filename = writeSound filename sound
        writeSounds :: [Sound] -> IO [String]
        writeSounds sounds = mapM (contentAddressableWrite "loop" "loops" "wav" . writer) sounds

--contentAddressableWrite :: String -> String -> (String -> IO ()) -> IO String

  -- let frameSize = 100
  -- msp $ map (uncurry (compareRms frameSize)) (zip loops spleeteredLoops)
  -- msp $ map (uncurry (rmsSimilarity frameSize)) (zip loops spleeteredLoops)

----withContentHashNamedFile :: String -> (String -> IO ()) -> (String -> IO a) -> IO a
--contentAddressableWrite :: Sound -> String
--contentAddressableWrite sound = withContentHashNamedFile writer returner
--  where writer filename = writeSound filename sound
--        returner filename = return filename

splitIntoLoops :: Sound -> [Int] -> [Sound]
splitIntoLoops sound bars =
  map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))

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

generateSome = do
  --ids <- search "drum tracks instrumental" 30
  filenames <- downloadMain "percussion isolated" 20
  let seeds = take 1 $ drop 3 $ take 10 [2885, 8834..]
  msp ("seeds", seeds)
  time "render" $ mapM (renderSong theArrayArrangement filenames) seeds

song :: Int -> IO ()
song seed = do
  loopFilenames <- fmap (map ("loops/"++)) $ listDirectory "loops"
  song <- renderSong theArrayArrangement loopFilenames seed
  writeSound ("song-" ++ (show seed) ++ ".wav") song

doStuffDefault = ["aff", "2345"]
doStuff ["bars", searchString, numTracks] = bars searchString (read numTracks)
doStuff ["song", seed] = song (read seed)
doStuff ["aff", seed] = affinityMain (read seed)
doStuff ["rpc"] = rpc
doStuff ["displayServer"] = displayServer
doStuff ["dm"] = displayMain
doStuff ["displaySend"] = withTerminal $ \d -> do
  displaySend d (Circ 70)
  return ()
doStuff [] = doStuff doStuffDefault
  --generateSome

main = withPortaudio $ do
  noBuffering
  gfxMain
  args <- getArgs
  msp $ "++ " ++ (show args)
  doStuff args
  msp "hi"
