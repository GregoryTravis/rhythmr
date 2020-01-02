module Main where

import Data.List.Utils (replace)
import System.Directory
import System.FilePath.Posix (takeBaseName)

import Arrangement
import Aubio
import Download
import Feh
import Mess
import Analysis
import Search
import Song
import Sound
import Spleeter
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
        searchStringDir = replace " " "-" searchString

getBars = do
  let frameSize = 1000
  filenames <- downloadMain "percussion isolated" 1
  barses <- mapM barBeat filenames
  loopses <- mapM (uncurry splitIntoLoops) (zip filenames barses)
  let loops = take 1 $ concat loopses -- TODO Surely some applicative can happen here
  msp $ map numFrames loops
  -- mapM (\(i, loop) -> writeSound ("loop-" ++ (show i) ++ ".wav") loop) (zip [0..] loops)
  -- msp filenames

  spleeteredLoops <- mapM (time "spleeter" . spleeter) loops
  --let spleeteredLoops = loops

  msp $ map (uncurry (compareRms frameSize)) (zip loops spleeteredLoops)
  msp $ map (uncurry (rmsSimilarity frameSize)) (zip loops spleeteredLoops)

splitIntoLoops :: String -> [Int] -> IO [Sound]
splitIntoLoops filename bars = do
  sound <- readSound filename
  return $ map (\(s, e) -> snip s e sound) (zip bars (drop 1 bars))

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

main = do
  noBuffering
  --generateSome
  getBars
  msp "hi"
