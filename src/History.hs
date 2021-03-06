{-# LANGUAGE DeriveGeneric #-}

module History
( History
, start
, toEnd
, toBeginning
, fromList
, toList
, update
, undo
, redo
, cur
, runEm
, hWhere
, crop ) where

import Data.Binary
import GHC.Generics (Generic)
--import Data.Traversable

import Util
import qualified Zipper as Z

data History s = History (Z.Zipper s)
  deriving (Eq, Read, Show, Generic)

instance Binary s => Binary (History s)

instance Functor History where
  fmap f (History z) = History (f <$> z)

--instance Traversable History where
--  traverse f (History z) = History (traverse f z)
----traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

start :: s -> History s
start = History . Z.makeZipper

toEnd :: History s -> History s
-- What am I doing wrong?
--toEnd h = Z.upFully <$> h
toEnd (History z) = History $ Z.upFully z

toBeginning :: History s -> History s
toBeginning (History z) = History $ Z.downFully z

fromList :: [s] -> History s
fromList = History . Z.fromList

toList :: History s -> [s]
toList (History z) = Z.toList z

update :: History s -> s -> History s
update (History z) s = History $ Z.push (Z.removeTop z) s

undo :: History s -> History s
undo (History z) = History $ Z.downMaybe z

redo :: History s -> History s
redo (History z) = History $ Z.upMaybe z

cur :: History s -> s
cur (History z) = Z.cur z

-- I feel somehow that this already exists
runEm :: History (IO a) -> IO (History a)
runEm (History z) = History <$> Z.runEm z

hWhere :: History s -> (Int, Int)
hWhere (History z) = Z.zwhere z

crop :: Int -> History a -> History a
--crop n h = fmap (Z.crop n) h -- Y not ok?
crop n (History z) = History (Z.crop n z)
