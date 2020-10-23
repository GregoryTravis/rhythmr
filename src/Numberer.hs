module Numberer
( Numberer
, empty
, add
, addMultiple
, toList
, fromList ) where

import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M

import Util

data Numberer a = Numberer Int (M.Map a Int)

empty :: Numberer a
empty = Numberer 0 M.empty

add :: Ord a => Numberer a -> a -> Numberer a
add num@(Numberer n m) x =
  case M.lookup x m of Just _ -> num
                       Nothing -> Numberer (n + 1) m'
    where m' = M.insert x n m

addMultiple :: Ord a => Numberer a -> [a] -> Numberer a
addMultiple num xs = foldl foo num xs
  where foo num x = add num x
--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

checkNumberer :: Numberer a -> b -> b
checkNumberer num@(Numberer n m) b =
  assertM "Numberer numbers not compact" ok b
    where ok = sort (M.elems m) == [0..n-1]

toList :: Ord a => Numberer a -> [a]
toList num@(Numberer n m) = checkNumberer num xs
  where xs = sortOn (m M.!) (M.keys m)
--sortOn :: Ord b => (a -> b) -> [a] -> [a]

fromList :: Ord a => [a] -> Numberer a
fromList = addMultiple empty
