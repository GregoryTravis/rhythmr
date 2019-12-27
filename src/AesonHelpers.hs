module AesonHelpers where

import Data.Aeson (Value(Array, String, Object))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import Util

objLookup (Object x) field = x HM.! (T.pack field)
objLookup x field = undefined
objLookupMaybe (Object x) field = fmap strGet $ HM.lookup (T.pack field) x
objHas (Object x) field = HM.member (T.pack field) x
objKeys (Object x) = HM.keys x
arrLookup (Array a) i = (V.toList a) !! i
arrGet (Array a) = a
arrMap f (Array a) = fmap f a
arrFilter f (Array a) = Array $ V.fromList $ filter f (V.toList a)
strGet (String t) = T.unpack t
one xs = let l = V.toList (arrGet xs)
          in assertM "one" (length l == 1) (l !! 0)
