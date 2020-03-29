{-# LANGUAGE DataKinds #-}

module Hypercube
( makeHypercube
, hypercubeMain
, projectHypercube ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (product)
import qualified Data.Map as M
import Data.Maybe
import Data.Vector (Vector, fromList, (!))
import qualified Data.Vector as V
import Linear
import Linear.V

import Util

type Pt = V 3 Double
numDims :: Int
numDims = 3

data Polytope = Polytope (Vector Pt) (Vector (Int, Int))
  deriving Show

mapVerts :: (Pt -> Pt) -> Polytope -> Polytope
mapVerts f (Polytope verts edges) = Polytope (V.map f verts) edges

getEdges :: Polytope -> Vector (Pt, Pt)
getEdges (Polytope verts edges) = V.map edge edges
  where edge (a, b) = (verts V.! a, verts V.!b)

makeHypercube :: Polytope
makeHypercube =
  let v0 :: [[Double]]
      v0 = expy (take numDims (repeat [0.0, 1.0]))
      v1 :: [V.Vector Double]
      v1 = map V.fromList v0
      v2 :: [Pt]
      v2 = map (fromJust . fromVector) v1 :: [Pt]
      verts :: V.Vector Pt
      verts = V.fromList v2
      vertToIndex :: M.Map Pt Int
      vertToIndex = M.fromList (zip (V.toList verts) [0..])
      edges :: Vector (Int, Int)
      edges = V.fromList $ nubOrd $ map sortEdge $ map vertToEdgeIndices $ concat (V.map vertToEdges verts)
      vertToEdges :: Pt -> [(Pt, Pt)]
      vertToEdges v = [(v, v') | v' <- adjacentVerts v]
      vertToEdgeIndices :: (Pt, Pt) -> (Int, Int)
      vertToEdgeIndices (a, b) = (vertToIndex M.! a, vertToIndex M.! b)
      sortEdge (a, b) | a > b = (b, a)
      sortEdge p = p
   in Polytope verts edges

-- Takes a pt with coordinates 0, 1 and return all adjacent pts
adjacentVerts :: Pt -> [Pt]
adjacentVerts pt =
  let ptV :: V.Vector Double
      ptV = toVector pt
      flippedPtsV :: [V.Vector Double]
      flippedPtsV = map flip (take (V.length ptV) [0..])
      flip :: Int -> V.Vector Double
      flip i = V.update ptV (V.fromList [(i, flipCoord (ptV V.! i))])
      flipCoord 0 = 1
      flipCoord 1 = 0
   in map (fromJust . fromVector) flippedPtsV

-- listToV :: [a] -> V n a
-- listToV = fromJust . fromVector . V.fromList

projectHypercube = undefined

hypercubeMain = do
  let p :: Polytope
      p = makeHypercube
  msp p

-- All possible ways to pick one element from each sublist
-- e.g. expy [[1, 2], [3, 4]] => [[1, 3], [1, 4], [2, 3], [2, 4]]
expy :: [[a]] -> [[a]]
expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- expy xss]
--expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- xss] ++ expy xss
expy [] = [[]]

--hypercubeMain = do
  -- let n = 8
  --     Just v = fromVector $ V.fromList (take n [2..]) :: Maybe (V 8 Int)
  --     Just m = fromVector $ V.fromList (take n (repeat v)) :: Maybe (V 8 (V 8 Int))
  -- msp v
  -- msp m
  -- let id = identity :: M44 Int
  -- msp id
  -- msp $ id !*! id
  -- msp $ m !*! m

--data Hypercube = Hypercube [Vector Double]
--  deriving Show

--hmap :: (Vector Double -> Vector Double) -> Hypercube -> Hypercube
--hmap f (Hypercube pts) = Hypercube (map f pts)

--makeHypercube :: Int -> Hypercube
--makeHypercube n = Hypercube $ map fromList vertses
--  where vertses = expy (take n (repeat [0.0, 1.0]))

---- All possible ways to pick one element from each sublist
---- e.g. expy [[1, 2], [3, 4]] => [[1, 3], [1, 4], [2, 3], [2, 4]]
--expy :: [[a]] -> [[a]]
--expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- expy xss]
----expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- xss] ++ expy xss
--expy [] = [[]]

--projectHypercube :: Hypercube -> [V2 Double]
----projectHypercube (Hypercube pts) = map projectPt pts
--projectHypercube = hmap projectPt

--translate :: Vector Double -> Hypercube -> Hypercube
--translate = hmap translatePt

--projectPt :: Vector Double -> V2 Double
--projectPt v =
--  let xy = V2 (v V.! 0) (v V.! 1)
--      zs = product (V.drop 2 v)
--   in xy ^/ zs

--hypercubeMain = do
--  let h = makeHypercube 3
--  msp h
--  let p = projectHypercube h
--  msp p
--  --msp $ expy [[1, 2], [3, 4]]
