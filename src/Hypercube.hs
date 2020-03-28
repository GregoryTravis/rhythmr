{-# LANGUAGE DataKinds #-}

module Hypercube
( makeHypercube
, hypercubeMain
, projectHypercube ) where

import Data.Maybe
import Data.Vector (Vector, fromList)
import Linear.V

import Util

data Hypercube = Hypercube [Vector Double]
  deriving Show

makeHypercube :: Int -> Hypercube
makeHypercube n = Hypercube $ map fromList vertses
  where vertses = expy (take n (repeat [0.0, 1.0]))

-- All possible ways to pick one element from each sublist
-- e.g. expy [[1, 2], [3, 4]] => [[1, 3], [1, 4], [2, 3], [2, 4]]
expy :: [[a]] -> [[a]]
expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- expy xss]
--expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- xss] ++ expy xss
expy [] = [[]]

projectHypercube :: Hypercube -> Hypercube
projectHypercube = undefined

hypercubeMain = do
  msp (makeHypercube 1)
  --msp $ expy [[1, 2], [3, 4]]
