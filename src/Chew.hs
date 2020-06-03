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
  rah

renderZGrid :: [[Zound]] -> IO Zound
renderZGrid zoundGrid = do
  let mix :: Zound
      mix = renderGrid zoundGrid bpm
  return mix

chew :: State -> IO Zound
chew s = do
  zg <- loadGrid s (S.toList (likes s))
  let [a, b] = zg !! 0
  song <- renderZGrid [[a], [b]]
  mix <- time "zrender" $ strictRender song
  return mix
