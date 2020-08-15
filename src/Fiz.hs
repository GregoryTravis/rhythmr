{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fiz
( Fiz
, update
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear
import System.Random

import Loop
import Util

type F = Double 
type Pos = V2 F
data Nudge a = Nudge a (V2 F)

speed :: F
speed = 1.0

-- positions: map from elements to locations.
data Fiz a = Fiz { positions :: M.Map a Pos }

-- Assumes we have a value for this element
getPos :: Ord a => Fiz a -> a -> Pos
getPos (Fiz {..}) x = positions M.! x

modPos :: Ord a => Fiz a -> (Pos -> Pos) -> a -> Fiz a
modPos fiz f x = Fiz { positions = M.insert x p' (positions fiz) }
  where p' = f (getPos fiz x)

-- Update elements to move towards the centers of gravity of the groups they are in.
update :: Ord a => [a] -> [[a]] -> Fiz a -> Fiz a
update xs groups f =
  let f' = updatePositionSet xs f
   in applyNudges f' (nudges f' groups)

-- The set of elements can change at each update. Remove all the elements that
-- are no longer in the element set, and for all new elements, add them with a
-- default position.
updatePositionSet :: Ord a => [a] -> Fiz a -> Fiz a
updatePositionSet xs fiz = Fiz positions'
  where positions' = M.fromList (zipWith (currentOrNew fiz) xs randomPositions)
        --currentOrNew :: Fiz a -> a -> Pos -> (a, Pos)
        currentOrNew fiz x deflt = (x, M.findWithDefault deflt x (positions fiz))
--findWithDefault :: Ord k => a -> k -> Map k a -> a
--maybe :: b -> (a -> b) -> Maybe a -> b

-- For each element in each group, calculate a delta that moves it towards the
-- cog of that group. Since an element can be in multiple groups, there can be
-- multiple nudges per element.
nudges :: Ord a => Fiz a -> [[a]] -> [Nudge a]
nudges f groups = concat (map (nudgeGroup f) groups)
nudgeGroup :: Ord a => Fiz a -> [a] -> [Nudge a]
nudgeGroup fiz xs = map (nudgeElement fiz cog) xs
  where cog = centerOfGravity fiz xs
nudgeElement :: Ord a => Fiz a -> Pos -> a -> Nudge a
nudgeElement fiz target x = Nudge x (nudgeFromTo (getPos fiz x) target)
nudgeFromTo :: Pos -> Pos -> Pos
nudgeFromTo x x' = x + (signorm (x' - x) ^* speed)

centerOfGravity :: Ord a => Fiz a -> [a] -> Pos
centerOfGravity fiz [] = error "centerOfGravity of empty list"
centerOfGravity fiz xs = sum poses / fromIntegral (length poses)
  where poses = map (getPos fiz) xs

-- Apply each nudge to its element
applyNudges :: Ord a => Fiz a -> [Nudge a] -> Fiz a
applyNudges = foldl applyNudge
--foldl :: (b -> a -> b) -> b -> t a -> b
applyNudge :: Ord a => Fiz a -> Nudge a -> Fiz a
applyNudge fiz (Nudge x v) = modPos fiz (+v) x

-- List of random positions. It's fine to use the same random positions over
-- and over, because the elements eventually move out of their original
-- positions.
randomPositions :: [Pos]
randomPositions =
  let rands = randomRs (-10, 10) (mkStdGen 37)
   in posUp rands
  where posUp :: [F] -> [Pos]
        posUp (x:y:rest) = V2 x y : posUp rest
