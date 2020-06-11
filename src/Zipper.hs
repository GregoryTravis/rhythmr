{-# LANGUAGE DeriveGeneric #-}

module Zipper
( Zipper
, makeZipper
, push
, cur
, up
, down
, upMaybe
, downMaybe
, removeTop
, zwhere
, toList
, fromList
, runEm ) where

import Data.Binary
import GHC.Generics (Generic)

-- This might not be a proper list zipper.
-- [1 2, 3, 4, 5, 6} with 4 selected is:
--   Zipper [3, 2, 1] 4 [5, 6]
--
-- The 'push' operation is poorly named. If we pushed 40 on the above zipper, we'd get
--   Zipper [3, 2, 1] 40 [4, 5, 6]

-- Zipper top current bottom; top is reversed
data Zipper a = Zipper [a] a [a]
  deriving (Eq, Read, Show, Generic)

instance Binary s => Binary (Zipper s)

instance Functor Zipper where
  fmap f (Zipper top cur bot) = Zipper (f <$> top) (f cur) (f <$> bot) 

--instance Foldable Zipper where
--  foldMap = foldMapDefault
--  --foldr (Zipper top cur bot) = folr
----foldr :: (a -> b -> b) -> b -> t a -> b

--instance Traversable Zipper where
--  traverse f (Zipper top cur bot) = do
--    top' <- traverse f top
--    cur' <- f cur
--    bot' <- traverse f bot
--    return (Zipper top' cur' bot')
----traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

--empty = Zipper [] []
makeZipper :: a -> Zipper a
makeZipper x = Zipper [] x []

cur (Zipper _ c _) = c

--set :: Zipper a -> a -> Zipper a
--set (Zipper top _ bot) x = Zipper top x bot

push :: Zipper a -> a -> Zipper a
push (Zipper top c bot) x = Zipper top x (c:bot)

up :: Zipper a -> Zipper a
up (Zipper (x:xs) c bot) = Zipper xs x (c:bot)

down :: Zipper a -> Zipper a
down (Zipper top c (x:xs)) = Zipper (c:top) x xs

upMaybe :: Zipper a -> Zipper a
upMaybe z@(Zipper [] _ _) = z
upMaybe z = up z

downMaybe :: Zipper a -> Zipper a
downMaybe z@(Zipper _ _ []) = z
downMaybe z = down z

removeTop :: Zipper a -> Zipper a
removeTop (Zipper _ c bot) = Zipper [] c bot

zwhere :: Zipper a -> (Int, Int)
zwhere (Zipper t _ b) = (length t, length b)

toList :: Zipper a -> [a]
toList (Zipper top c bot) = top ++ [c] ++ bot

-- Foous is initialized to the head of the list
fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

-- Tried to use Traversable, but why is it a subclass of Foldable? Baffled
runEm :: Zipper (IO a) -> IO (Zipper a)
runEm (Zipper top cur bot) = do
  Zipper <$> runList top <*> cur <*> runList bot

runList :: [IO a] -> IO [a]
runList (io : ios) = do
  (:) <$> io <*> runList ios
runList [] = return []
