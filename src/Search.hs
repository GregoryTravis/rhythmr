module Search (search) where

import Data.Aeson (Value(Array, String, Object))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Exts

import External
import Util

-- todo: overloaded strings

search = do
  Just d <- cachedJsonCommand "python" ["get-mp3s.py"]
  let items = objLookup d "items"
  return $ map strGet $ V.toList $ arrMap getId (arrFilter isVideo items)
  where objLookup (Object x) field = x HM.! (T.pack field)
        arrLookup (Array a) i = (V.toList a) !! i
        arrMap f (Array a) = fmap f a
        arrFilter f (Array a) = Array $ V.fromList $ filter f (V.toList a)
        strGet (String t) = T.unpack t
        getId item = objLookup (objLookup item "id") "videoId"
        isVideo item = strGet (objLookup (objLookup item "id") "kind") == "youtube#video"
