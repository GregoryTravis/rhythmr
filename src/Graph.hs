{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Graph
( Graph
, empty
, nullGraph
, connectedTo
, add
, addMulti
, nodes
, components
, fromComponents
, showGraphAsComponents
, longestPathComponent
, thresholdedWalks
, MetaGraph
, buildMetaGraph
, graphInfo
, graphStruct
, graphTest ) where

---- Really dumb undirected graph: extremely slow!!

import Control.DeepSeq
import GHC.Generics (Generic, Generic1)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, intersect, maximum, nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Util

-- An unordered graph expressed as a map from each node to all the nodes it
-- shares an edge with. Each edge is represented twice, once for each of its
-- nodes. This representation does not permit nodes without any edges attached
-- to them.

data Graph a = Graph (M.Map a (S.Set a))
  deriving (Eq, Generic)

instance NFData a => NFData (Graph a)

instance (Eq a, Ord a, Show a) => Show (Graph a) where
  show g = show $ edges g

empty :: Graph a
empty = Graph (M.empty)

nullGraph :: Graph a -> Bool
nullGraph (Graph m) = M.null m

graphInfo :: (Show a, Ord a) => Graph a -> (Int, Int, Double, Int)
graphInfo g = (length $ nodes g, length $ edges g, density, length $ components g)
  where density = (fromIntegral $ length $ edges g) / (fromIntegral $ sq $ length $ nodes g)
        sq x = x * x

graphStruct :: Ord a => Graph a -> [(Int, [Int])]
graphStruct g =
  let ns = S.toList (nodes g)
      elemToInt = M.fromList (zip ns [0..])
      lup x = elemToInt M.! x
      showNode n = (lup n, map lup (connectedTo g n))
   in map showNode ns

-- Add an edge (x, y). Adds y to the adjacency list for x, and vice versa,
-- because I'm a jerk.
add :: (Ord a, Show a) => Graph a -> a -> a -> Graph a
add g x y =
  let g' = addKeyIfMissing g x
      g'' = addKeyIfMissing g' y
      (Graph m) = g''
      m' = M.adjust (S.insert x) y m
      m'' = M.adjust (S.insert y) x m'
   -- in eesp (show ("hoy", m, m', m'')) $ Graph m''
   in Graph m''

-- Add multiple edges.
-- TODO this is a fold
addMulti :: (Ord a, Show a) => Graph a -> [(a, a)] -> Graph a
addMulti g ((x, y) : ps) = addMulti (add g x y) ps
addMulti g [] = g

-- Add the given elements as a connected component: given (x:ys), add (x, y)
-- for each y in ys.
addComponent :: (Ord a, Show a) => Graph a -> [a] -> Graph a
addComponent g (x:xs) = addMulti g (zip (repeat x) xs)
addComponent g [] = g

edges :: (Eq a, Ord a) => Graph a -> [(a, a)]
edges g = nubOrd $ map sortEdge $ directedEdges g

sortEdge :: Ord a => (a, a) -> (a, a)
sortEdge (a, b) | a > b = (b, a)
sortEdge (a, b) | otherwise = (a, b)

-- Return each edge twice, in each ordering
directedEdges :: Ord a => Graph a -> [(a, a)]
directedEdges g@(Graph m) = concat (Prelude.map (nodeEdges g) (M.keys m))

-- Return all nodes connected by an edge to the given node
connectedTo :: Ord a => Graph a -> a -> [a]
connectedTo (Graph m) x = S.toList (m M.! x)

-- Return all edges (x, y) for the given x
nodeEdges :: Ord a => Graph a -> a -> [(a, a)]
nodeEdges g x = map (x,) $ (connectedTo g x)

-- Return connected components of the graph
-- This is extremely inefficient; it constructs a size-n component n times
components :: (Eq a, Ord a, Show a) => Graph a -> [S.Set a]
components g = nub $ Prelude.map (closure g) (S.toList (nodes g))

showComponents :: Show a => [S.Set a] -> String
showComponents sets = intercalate " " $ map show (map S.toList sets)

showGraphAsComponents :: (Eq a, Ord a, Show a) => Graph a -> String
showGraphAsComponents = showComponents . components

-- Construct a graph from the given connected components.
-- They don't have to be disjoint, so (components . fromComponents) /= id
fromComponents :: (Show a, Ord a) => [[a]] -> Graph a
fromComponents [] = Graph M.empty
fromComponents (c:cs) = addComponent (fromComponents cs) c

nodes :: Ord a => Graph a -> S.Set a
nodes (Graph m) = flatten (M.elems m)

closure :: (Ord a, Show a) => Graph a -> a -> S.Set a
closure g x = converge (closure' g) (S.singleton x)

closure' :: (Ord a, Show a) => Graph a -> S.Set a -> S.Set a
closure' (Graph m) xs = xs `S.union` (flatten $ Prelude.map (m M.!) (S.toList xs))

flatten :: Ord a => [S.Set a] -> S.Set a
flatten sets = S.fromList (concat (Prelude.map S.toList sets))

-- I wonder if this doesn't recompute (f x)
converge :: Eq a => (a -> a) -> a -> a
converge f x | (f x) == x = x
converge f x | otherwise = converge f (f x)

addKeyIfMissing :: Ord a => Graph a -> a -> Graph a
addKeyIfMissing g x | graphMember x g = g
addKeyIfMissing (Graph m) x | otherwise = Graph $ M.insert x S.empty m

graphMember :: Ord a => a -> Graph a -> Bool
graphMember x (Graph m) = M.member x m

-- -- Starting at the given element, walk the connectivity tree emanating from it,
-- -- avoiding cycles.
-- walkAndCount :: (Show a, Ord a) => Graph a -> a -> [(a, Int)]
-- walkAndCount g x = walk S.empty 0 g x
--   where walk :: (Show a, Ord a) => S.Set a -> Int -> Graph a -> a -> [(a, Int)]
--         walk seen n g x = (x, n) : (concat (map (walk seen' (n+1) g) nexts))
--           where nexts = S.toList ((S.fromList (connectedTo g x)) `S.difference` seen)
--                 seen' = S.insert x seen

-- Starting at the given element, walk the connectivity tree emanating from it,
-- avoiding cycles. Returns all paths.
pathsFrom :: (Show a, Ord a) => Graph a -> a -> [[a]]
pathsFrom g x = walk S.empty g x
  where walk :: (Show a, Ord a) => S.Set a -> Graph a -> a -> [[a]]
        walk seen g x = map (x:) ([] : (concat (map (walk seen' g) nexts)))
          where nexts = S.toList ((S.fromList (connectedTo g x)) `S.difference` seen)
                seen' = S.insert x seen

-- Return a longest path from a. If there are multiple paths of that length,
-- the first one found is returned.
longestPathFrom :: (NFData a, Show a, Ord a) => Graph a -> a -> [a]
longestPathFrom g x =
  let paths = tv "pathsFrom " $ pathsFrom g x
      maxLength = maximum (map length (check paths))
      check paths = assertM "longestPathFrom" (not (null paths)) paths
      longestPaths = filter ((== maxLength) . length) paths
   in head longestPaths

-- Find the longest path in a connected component.  If there are multiple paths
-- of that length, the first one found is returned.
longestPathComponent :: (NFData a, Show a, Ord a) => Graph a -> a -> [a]
longestPathComponent g x =
  let firstPath = longestPathFrom g x
      secondStartingPoint = last firstPath
      secondPath = longestPathFrom g secondStartingPoint
   in secondPath

-- Find a longest path through each component, and concatenate them.
-- If the argument is an empty graph, then an empty list is returned.
allLongestPathComponents :: (NFData a, Show a, Ord a) => Graph a -> [a]
allLongestPathComponents g =
  let cs = map S.toList (tv "components" $ components g)
      startingPoints = map head cs
      longestPaths = map (longestPathComponent g) startingPoints
   in concat longestPaths

-- Separate module?
type MetaGraph a = Graph [a]

-- Construct a k-metagraph.
-- Such a graph has an edge (x, y) if the intersection of x and y is of size k or greater.
buildMetaGraph :: (Eq a, Show a, Ord a) => [[a]] -> Int -> MetaGraph a
buildMetaGraph xses k = addMulti empty (findOverlappingBy k xses)

-- Return pairs of lists that overlap by the specified number of elements
-- (excluding self-overlapping)
findOverlappingBy :: (Eq a, Show a, Ord a) => Int -> [[a]] -> [([a], [a])]
findOverlappingBy k xses | k >= 1 = filter (uncurry ok) (findOverlapping xses)
                         | otherwise = error ("findOverlapping: k must be >= 1, is " ++ (show k))
  where ok xs ys = overlapBy k xs ys && (xs /= ys)

-- Return pairs of lists that overlap by at least one element.
findOverlapping :: (Eq a, Show a, Ord a) => [[a]] -> [([a], [a])]
findOverlapping xses =
  let e2l = elementToListsMap xses
      allPairs xs = [(x, y) | x <- xs, y <- xs]
   in sfesp "uniq" length $ nubOrd $ sfesp "all" length $ concat $ map allPairs (M.elems e2l)

-- Build a map from each element to the lists that contain it
elementToListsMap :: (Eq a, Ord a) => [[a]] -> M.Map a [[a]]
elementToListsMap xses = mapFromListAccum $ concat (map kvs xses)
  where kvs :: [a] -> [(a, [a])]
        kvs xs = map (,xs) xs

-- Nonempty intersection?
overlapBy :: Eq a => Int -> [a] -> [a] -> Bool
overlapBy k xs ys = length (intersect xs ys) >= k

-- For each k >= 1, build the k-metagraph and return (k, walk). Stop when the
-- walks become empty.
thresholdedWalks :: (NFData a, Show a, Ord a) => [[a]] -> [(Int, [[a]])]
thresholdedWalks xses = nonEmpty
  where walks = map walk [0..]
        walk k = (k, tv "allLongestPathComponents" $ allLongestPathComponents (mg k))
        mg k = tv "buildMetaGraph" $ buildMetaGraph xses k
        nonEmpty = tv "nonEmpty" $ takeWhile (\x -> tv "x" $ ((\(_, walk) -> not (null walk)) x)) walks
        --evaled = unsafeTime "thresholdedWalks" nonEmpty
        -- debug = map d [0..4]
        --   where d k =
        --           let mg = buildMetaGraph xses k
        --               cs = components mg
        --               adjs = case mg of Graph m -> map length (M.elems m)
        --            in ("debug", k, adjs, map length cs)
        -- debug = map (\(k, walk) -> (k, length walk)) $ take 20 walks
        --debug = map graphInfo (map (buildMetaGraph xses) [0..14])
        -- debug = allLongestPathComponents (Graph.empty :: Graph Int)
        -- graphInfo (Graph m) = ("gi", map length (M.elems m))

graphTest :: IO ()
graphTest = do
  let likes =
        [ [0, 1, 2::Int] -- Int here for NFData while debugging performance
        , [1, 3, 4]
        , [3, 2, 5]
        , [5, 1, 3]
        , [10, 11, 12]
        , [10, 13, 14] ]
      mg = buildMetaGraph likes 1
      walked = pathsFrom mg [0, 1, 2]
  msp $ length walked
  msp $ length $ nubOrd walked
  msp $ map length walked
  msp $ longestPathComponent mg [0, 1, 2]
  --msp $ nubOrd walked
  --msp mg
  -- let m = case mg of (Graph m) -> m
  -- msp (length (M.keys m))
  -- mapM_ (\k -> msp (k, (S.size (m M.! k)))) (M.keys m)
