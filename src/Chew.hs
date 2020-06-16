{-# LANGUAGE NamedFieldPuns #-}

module Chew
( chew ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import qualified Data.Set as S

import Constants
import Loop
import Looper
import State
import Util
import Zounds

renderLoopGrid :: State -> [[Loop]] -> IO Zound
renderLoopGrid (State { soundLoader }) loopGrid = do
  let numLoops = length (nubOrd (concat loopGrid))
  msp ("loopgrid", numLoops)
  let filenameGrid :: [[String]]
      filenameGrid = map (map loopFilename) loopGrid
      rah :: IO [[Zound]]
      rah =  mapM (mapM soundLoader) filenameGrid
  zoundGrid <- ((mapM (mapM soundLoader) filenameGrid) :: IO [[Zound]])
  let mix :: Zound
      mix = renderGrid zoundGrid bpm
  return mix

-- Loads and resamples to standard length
loadGrid :: State -> [[Loop]] -> IO [[Zound]]
loadGrid s loopGrid = do
  let numLoops = length (nubOrd (concat loopGrid))
  msp ("loopgrid", numLoops)
  let filenameGrid :: [[String]]
      filenameGrid = map (map loopFilename) loopGrid
      rah :: IO [[Zound]]
      rah =  mapM (mapM (soundLoader s)) filenameGrid
      -- rooh :: IO [[Zound]]
      -- rooh = mapM (mapM (ExternalFx (resampleZoundProcessor loopLengthFrames))) rah
      -- reh :: IO [[Zound]]
      -- reh = mapM (mapM render) rooh
  ugh <- rah
  let ugh' :: [[Zound]]
      ugh' = map (map (ExternalFx (resampleZoundProcessor loopLengthFrames))) ugh
  mapM (mapM render) ugh'

renderZGrid :: [[Zound]] -> Zound
renderZGrid zses =
  let offsets = take (length zses) (map (* loopLengthFrames) [0..])
      stacks = map Mix zses
   in Mix $ zipWith Translate offsets stacks

-- Chop into n pieces and translate all to origin
slice :: Int -> Zound -> [Zound]
slice n z =
  let Bounds s e = getBounds z
      interp :: Double -> Frame
      interp x = floor $ ((1.0 - x) * fromIntegral s) + (x * fromIntegral e)
      points :: [Frame]
      points = map floor $ map (* (fromIntegral e - fromIntegral s)) $ map ((/ fromIntegral n) . fromIntegral) [0..n]
      starts = take n points
      ends = drop 1 points
      pieces = zipWith (\s e -> snip s e z) starts ends
   in pieces

-- Chop into n pieces and translate all to origin
dice :: Int -> Zound -> [Zound]
dice n z = map toZero (slice n z)

isAtOrigin :: Zound -> Bool
isAtOrigin z = getStart (getBounds z) == 0

areAtOrigin :: [Zound] -> Bool
areAtOrigin zs = all isAtOrigin zs

-- Must be s=0
seqZounds :: [Zound] -> Zound
seqZounds zs | areAtOrigin zs =
  -- let offsets = take (length zs) $ (0 : (map getEnd $ map getBounds zs))
  --     translated = zipWith Translate offsets zs
  let translated = translateList 0 zs
   in Mix translated
  where translateList :: Int -> [Zound] -> [Zound]
        translateList dt (z : zs) = (Translate dt z) : translateList (dt + getEnd (getBounds z)) zs
        translateList dt [] = []

-- Apply a transform to the zound starts
mapStarts :: (Frame -> Frame) -> [Zound] -> [Zound]
mapStarts f zs = map update zs
  where update z = Translate dt z
          where s = getStart (getBounds z)
                dt = (f s) - s

-- stretch the start-points of the sound relative to the origin
scaleSeq :: Double -> Zound -> Zound
scaleSeq scale (Mix zs) = Mix $ mapStarts (floor . (* scale) . fromIntegral) zs

sameBounds :: (Zound -> Zound) -> (Zound -> Zound)
sameBounds f z = Bounded (getBounds z) (f z)

chopOut :: Int -> [Int] -> Zound -> Zound
chopOut n keepers z = Mix $ zipWith keepOrSilence [0..n-1] (slice n z)
  where keepOrSilence i sz | elem i keepers = sz
        keepOrSilence i sz | otherwise = Silence (getBounds sz)
       
-- chopOut n keepers z = Mix (map (pieces !!) keepers)
--   where pieces = slice n z

addClick :: Zound -> [[Zound]] -> [[Zound]]
addClick clik = map (++ [clik])

chopOuts :: [(Int, [Int])]
chopOuts =
  [ (4, [0, 2])
  , (8, [0, 2, 4, 6])
  , (4, [1, 3])
  , (8, [1, 3, 5, 7])
  , (4, [0, 3])
  , (8, [0, 5])
  , (8, [3, 7, 8])
  ]

-- Use the first loop intact; chop out the rest
wackyStack :: [Zound] -> Zound
wackyStack [] = error "empty wackyStack"
wackyStack (z:zs) = Mix $ [z] ++ zipWith co (cycle chopOuts) zs
  where co (n, keepers) z = chopOut n keepers z

-- Rotate the stack a few times
wackyStacks :: [Zound] -> [Zound]
wackyStacks zs = map wackyStack $ map (flip rotate zs) [0..n-1]
  where n = 3

chew :: State -> IO Zound
chew s = do
  clik <- readZound "wavs/clik.wav"
  --let loops = S.toList (likes s)
  let loops = affinities s
  likes' <- loadGrid s loops
  let likes = reverse $ sortOn length likes'
  let grid = map (:[]) $ concat (map wackyStacks likes)
  let song = renderZGrid $ addClick clik grid
  mix <- time "zrender" $ strictRender song
  writeZound "chew.wav" mix
  return mix
