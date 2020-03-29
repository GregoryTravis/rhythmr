{-# LANGUAGE DataKinds #-}

module Hypercube
( Polytope
, makeHypercube
, hypercubeMain
, projectPolytope
, rotatePolytope
, showIt ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (product, splitAt)
import qualified Data.Map as M
import Data.Maybe
import Data.Vector (Vector, fromList, (!))
import qualified Data.Vector as V
import Linear
import Linear.V

import Util

type VN a = V 8 a
numDims :: Int
numDims = 8
type Pt = VN Double
type Mat = VN (VN Double)

data Polytope = Polytope (Vector Pt) (Vector (Int, Int))
  deriving Show

mapVerts :: (Pt -> Pt) -> Polytope -> Polytope
mapVerts f (Polytope verts edges) = Polytope (V.map f verts) edges

-- mapVertsV :: (Vector Double -> Vector Double) -> Polytope -> Polytope
-- mapVertsV f = mapVerts (fromJust . fromVector . f . toVector)

getVerts :: Polytope -> Vector Pt
getVerts (Polytope verts _) = verts

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
      centerIt :: Polytope -> Polytope
      centerIt = mapVerts (trans+)
        where trans = fromJust $ fromVector $ V.fromList (take numDims $ repeat (-0.5)) :: Pt
   in centerIt $ Polytope verts edges

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

-- Project the edges of the polytope to 2D.
-- Divide the first two coordinates by the product of the others
projectPolytope :: Polytope -> [(V2 Double, V2 Double)]
projectPolytope p@(Polytope verts edges) = map projEdge $ V.toList edges
  where projEdge (a, b) = (projectedPts V.! a, projectedPts V.! b)
        projectedPts = V.map projectPt verts
        --yah = ("edges", getEdges p)

inFront :: Pt -> Bool
inFront pt = z >= 1.0
  where z = (V.toList $ toVector pt) !! 2

projectPt :: Pt -> V2 Double
projectPt pt | inFront pt =
  let ptList = V.toList $ toVector pt
      ([x, y], theRest) = splitAt 2 ptList
      prod = product theRest
   in V2 (x / prod) (y / prod)

vecVecToMat :: Vector (Vector Double) -> Mat
vecVecToMat vs = fromJust $ fromVector (V.map (fromJust . fromVector) vs :: Vector Pt) :: Mat

listListToMat :: [[Double]] -> Mat
listListToMat dses = vecVecToMat $ V.fromList (map V.fromList dses)

-- Rotate in the plane specified by the pair of axis numbers
mkRotation :: Double -> Int -> Int -> Mat
mkRotation ang a b = listListToMat nses
  where nses = [[val a' b' | a' <- [0..numDims-1]] | b' <- [0..numDims-1]]
        val a' b' | a == a' && a == b' = c
        val a' b' | b == a' && b == b' = c
        val a' b' | b == a' && a == b' = (-s)
        val a' b' | a == a' && b == b' = s
        val a' b' | a' == b' = 1
        val a' b' | otherwise = 0
        c = cos ang
        s = sin ang

-- All possible ways to pick one element from each sublist
-- e.g. expy [[1, 2], [3, 4]] => [[1, 3], [1, 4], [2, 3], [2, 4]]
expy :: [[a]] -> [[a]]
expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- expy xss]
--expy (xs : xss) = [x' : xs' | x' <- xs, xs' <- xss] ++ expy xss
expy [] = [[]]

-- The formula for far isn't right; it should produce approx the same size
-- projection at any dimension, but it doesn't.
moveAway :: Pt
moveAway = fromJust $ fromVector $ V.fromList allButXY
  where allButXY = [0, 0] ++ (take (numDims - 2) (repeat far))
        far = 50 ** (1 / fromIntegral (numDims - 2))
-- Wrong! I was only pushing along Z, but I should have been pushing along *all but x and y*
-- where justZ = esp $ (V.fromList (take numDims (repeat 0))) `V.update` (V.fromList [(2, 20)])

--(Data.Vector.fromList (take 4 (repeat 0))) `Data.Vector.update` (Data.Vector.fromList [(2, 5)])

rotatePolytope :: Double -> Int -> Int -> Polytope -> Polytope
rotatePolytope ang a b p = mapVerts (mkRotation ang a b !*) p

showIt :: Polytope -> Polytope
showIt = mapVerts (moveAway +)

hypercubeMain = do
  let p :: Polytope
      p = makeHypercube
      turn :: Mat
      turn = mkRotation (pi/4) 0 1
      both :: Pt -> Pt
      both pt = moveAway + (turn !* pt)
      p' = mapVerts both p
      proj = projectPolytope p'
  msp $ turn !*! turn
  msp $ [moveAway] !*! turn
  msp p
  msp moveAway
  msp proj
  --mapM_ msp (getVerts p)
  mapM_ (msp . uncurry (-)) (getEdges p)
  --msp $ hist $ map (uncurry (-)) $ V.toList (getEdges p)

  -- let n = 3
  --     v :: V 3 Int
  --     v = fromJust $ fromVector $ V.fromList (take n [2..])
  --     m :: V 3 (V 3 Int)
  --     m = fromJust $ fromVector $ V.fromList (take n (repeat v))
  -- msp v
  -- msp m
  -- let id = identity :: M44 Int
  -- msp id
  -- msp $ id !*! id
  -- msp $ m !*! m
  -- msp $ [v] !*! m
  -- msp $ m !* v
