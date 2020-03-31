{-# LANGUAGE DataKinds #-}

module Hypercube
( Polytope
, Pt
, Mat
, makeHypercube
, hypercubeMain
, projectPolytope
, rotatePolytope
, applyMatrix
, rotateTowards
, getVerts
, pointingAtCamera
, numDims
, moveAway
, projectPt
, showIt ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (product, sortBy, sortOn, splitAt)
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple (swap)
import Data.Vector (Vector, fromList, (!))
import qualified Data.Vector as V
import Linear
import Linear.V

import Util

type VN a = V 3 a
numDims :: Int
numDims = 3
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

pointingAtCamera = fromJust $ fromVector (V.fromList ([0, 0, -0.5] ++ take (numDims-3) (repeat 0))) :: Pt

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

envelope2 :: (Double -> Double -> Bool) -> Pt -> Pt -> Pt
envelope2 cmp a b =
  let al = V.toList (toVector a)
      bl = V.toList (toVector b)
      maxes = zipWith (\a b -> if a `cmp` b then a else b) al bl
   in fromJust $ fromVector (V.fromList maxes) :: Pt

envelope :: (Double -> Double -> Bool) -> [Pt] -> Pt
envelope cmp = foldr1 (envelope2 cmp)

boundingBox :: Polytope -> (Pt, Pt)
boundingBox p =
  let verts = getVerts p
   in (envelope (<) (V.toList verts), envelope (>) (V.toList verts))

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
projectPt pt | otherwise = eesp ("not in front", pt) undefined

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
moveAway = eeesp "moveAway" $ fromJust $ fromVector $ V.fromList allButXY
  where allButXY = [0, 0] ++ (take (numDims - 2) (repeat far))
        far = 5 -- 100 ** (1 / fromIntegral (numDims - 2))
-- Wrong! I was only pushing along Z, but I should have been pushing along *all but x and y*
-- where justZ = esp $ (V.fromList (take numDims (repeat 0))) `V.update` (V.fromList [(2, 20)])

--(Data.Vector.fromList (take 4 (repeat 0))) `Data.Vector.update` (Data.Vector.fromList [(2, 5)])

rotatePolytope :: Double -> Int -> Int -> Polytope -> Polytope
rotatePolytope ang a b p = mapVerts (mkRotation ang a b !*) p

applyMatrix :: Mat -> Polytope -> Polytope
applyMatrix m p = mapVerts (m !*) p

showIt :: Polytope -> Polytope
showIt = mapVerts (moveAway +)

allPlanes :: [(Int, Int)]
allPlanes = (allPairs [0..numDims-1])

-- All axes (planes) that include at least one of the 3 visible ones
planesSomeVisible :: [(Int, Int)]
planesSomeVisible = planes ++ swapped
  where hasXYOrZ (a, b) = a <= 2 || b <= 2
        planes = filter hasXYOrZ allPlanes
        swapped = map swap planes

-- Rotation bringing one vector towards another. Try a small rotation along
-- each 2-plane that includes at least one of the three visible axes, and
-- measure how much closer src got to dest via that rotation. Score each
-- rotation by how much closer src gets to dest (comparing dot products). If
-- the score is negative, use its reverse. Then take the n best ones and blend
-- them, which means to concatenate them in an arbitrary but consistent order,
-- by the default ordering on the planes. Scale the rotations by 1/n of the
-- supplied angle.
--
-- opt: this calculates all the rots, then picks the best and recalculates
-- them, since some of them were reversed.
rotateTowards :: Double -> Pt -> Pt -> Mat
rotateTowards dt src dest | coincide src dest = identity
rotateTowards ang src dest = eesp debug concatenated
  where concatenated :: Mat
        concatenated = foldr (!*!) identity bestN
        origDot :: Double
        origDot = (signorm src) `dot` (signorm dest)
        bestCount :: Int
        bestCount = 2
        angFrac :: Double
        angFrac = ang / fromIntegral bestCount
        bestN :: [Mat]
        bestN = map (uncurry $ mkRotation angFrac) $ map snd $ take bestCount scored
        planes :: [(Int, Int)]
        planes = allPlanes
        rots :: [Mat]
        rots = map (uncurry $ mkRotation smallAng) planes
        scores :: [Double]
        scores = map score rots
        score :: Mat -> Double
        score m = thisDot - origDot
          where thisDot = (signorm (m !* src)) `dot` (signorm dest)
        scored :: [(Double, (Int, Int))]
        scored = reverse $ sortOn fst $ map flipBad (zip scores planes)
        --scored = sortOn fst $ map flipBad $ score planes
        flipBad (s, (a, b)) | s < 0 = (-s, (b, a))
        flipBad p | otherwise = p
        smallAng = pi / 16
        debug = ("D", ang, src, dest, scored)

-- -- Rotation bringing one vector towards another. Try a small rotation along
-- -- each axis, and check whether or not the vector got closer or further away.
-- -- Use the axis that got it the closest. By axis I really mean plane, defined
-- -- by two axes.
-- rotateTowards :: Double -> Pt -> Pt -> Mat
--rotateTowards dt src dest | coincide src dest = identity
--rotateTowards dt src dest | otherwise = {-eesp debug $-} newRot
--  where allPlanes :: [(Int, Int)]
--        allPlanes = allPairs [0..numDims-1]
--        somePlanes :: [(Int, Int)]
--        --somePlanes = [(0, 1), (0, 2), (1, 2)]
--        somePlanes = planesSomeVisible
--        smallRot :: (Int, Int) -> Mat
--        smallRot (a, b) = mkRotation ang a b
--        planesAndScores :: [((Int, Int), Double)]
--        --planesAndScores = map absFlip $ map (\x -> (x, score x)) somePlanes
--        planesAndScores = map (\x -> (x, score x)) somePlanes
--        best :: (Int, Int)
--        best = fst $ bestBy snd planesAndScores
--        absFlip :: ((Int, Int), Double) -> ((Int, Int), Double)
--        absFlip ((a, b), n) | n < 0 = ((b, a), (-n))
--        absFlip x | otherwise = x
--        score :: (Int, Int) -> Double
--        score (a, b) = {-eeesp (a, b) $-} (m !* src) `dot` dest
--          where m = smallRot (a, b)
--        use (a, b) = mkRotation ang a b
--        ang = (pi/denom) * dt
--        newRot = use best
--        debug = ("RT", best, sd < sd', sd, sd', src, dest, src, newRot)
--          where src' = newRot !* src
--                sd = src `dot` dest
--                sd' = src' `dot` dest
--        -- debug = ("RT", src `dot` dest, src' `dot` dest, src, dest, src')
--        --   where src' = (use best) !* src
--        denom = 16

-- True if the points are close enough together
coincide :: Pt -> Pt -> Bool
coincide a b = (signorm a) `dot` (signorm b) > 0.99

-- Get a score for each element and return the value with the highest score
bestBy :: Ord s => (a -> s) -> [a] -> a
bestBy scorer xs =
  let pairs = zip xs (map scorer xs)
      sorted = sortBy cmp pairs
      cmp (x, score) (x', score') = compare score score'
   in fst $ head (reverse sorted)

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
