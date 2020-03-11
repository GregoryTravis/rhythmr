module History
( History
, History.init
, fromList
, toList
, update
, undo
, redo
, cur ) where

import Util
import qualified Zipper as Z

data History s = History (Z.Zipper s)

init :: s -> History s
init = History . Z.makeZipper

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
