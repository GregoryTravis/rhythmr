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

loadGrid :: State -> [[Loop]] -> IO [[Zound]]
loadGrid s loopGrid = do
  let numLoops = length (nubOrd (concat loopGrid))
  msp ("loopgrid", numLoops)
  let filenameGrid :: [[String]]
      filenameGrid = map (map loopFilename) loopGrid
      rah :: IO [[Zound]]
      rah =  mapM (mapM (soundLoader s)) filenameGrid
  ugh <- rah
  mapM (mapM render) ugh

renderZGrid :: [[Zound]] -> Zound
renderZGrid zoundGrid = renderGrid zoundGrid bpm

-- Chop into n pieces
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
   in eesp ("oy", s, e, points, starts, ends) pieces

chew :: State -> IO Zound
chew s = do
  zg <- loadGrid s (S.toList (likes s))
  let [a, b] = zg !! 0
  --let da = Mix $ reverse $ dice 2 a
  let [a0, a1] = dice 2 a
  let song = renderZGrid [[a], [a], [a1], [a0]]
  mix <- time "zrender" $ strictRender song
  return mix
