module History
( History
, start
, fromList
, toList
, update
, undo
, redo
, cur
, runEm
) where

--import Data.Traversable

import Util
import qualified Zipper as Z

data History s = History (Z.Zipper s)
  deriving (Eq, Read, Show)

instance Functor History where
  fmap f (History z) = History (f <$> z)

--instance Traversable History where
--  traverse f (History z) = History (traverse f z)
----traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

start :: s -> History s
start = History . Z.makeZipper

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
runEm (History z) = do
  z' <- Z.runEm z
  return $ History z'
