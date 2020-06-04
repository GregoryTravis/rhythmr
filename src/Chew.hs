{-# LANGUAGE NamedFieldPuns #-}

module Chew
( chew ) where

import Data.Containers.ListUtils (nubOrd)
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
renderZGrid zoundGrid = renderGrid zoundGrid bpm

-- Chop into n pieces and translate all to origin
dice :: Int -> Zound -> [Zound]
dice n z =
  let Bounds s e = getBounds z
      interp :: Double -> Frame
      interp x = floor $ ((1.0 - x) * fromIntegral s) + (x * fromIntegral e)
      points :: [Frame]
      points = map floor $ map (* (fromIntegral e - fromIntegral s)) $ map ((/ fromIntegral n) . fromIntegral) [0..n]
      starts = take n points
      ends = drop 1 points
      pieces = zipWith (\s e -> snip s e z) starts ends
   in eesp ("oy", s, e, points, starts, ends) $ map toZero pieces

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

chew :: State -> IO Zound
chew s = do
  likes <- loadGrid s (S.toList (likes s))
  let [a, b] = likes !! 0
  msp a
  msp b
  let a' = seqZounds (reverse (dice 4 a))
  msp a'
  --let da = Mix $ reverse $ dice 2 a
  let song = renderZGrid [[a], [a], [a'], [a']]
  mix <- time "zrender" $ strictRender song
  return mix
