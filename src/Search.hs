module Search (search, searchNoPaging) where

import Data.Aeson (Value(Array, String, Object))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Exts

import External
import Util

-- todo: overloaded strings

-- osearch :: String -> Int -> IO [String]
-- osearch searchString count = search' searchString count 0 Nothing

-- search' :: String -> Int -> Int -> (Maybe String) -> IO [String]
-- search' searchString count soFar pageToken | soFar >= count = return []
-- search' searchString count soFar pageToken | otherwise = do
--   (pageToken, some) <- search'' pageToken searchString (count - soFar)
--   msp ("welp", count, soFar, length some)
--   theRest <- search' searchString count (soFar + (length some)) (Just pageToken)
--   return $ some ++ theRest

searchNoPaging :: String -> Int -> IO [String]
-- TODO I think this could be something like (fmap snd . search' Nothing)
searchNoPaging searchString count = do
  (_, ids) <- search' Nothing searchString count
  return ids

search' :: Maybe String -> String -> Int -> IO (String, [String])
search' pageToken searchString count = do
  msp ("hey", pageToken, searchString, count)
  Just d <- cachedJsonCommand "python" ["get-mp3s.py", searchString, show count, pageTokenParam pageToken]
  let items = objLookup d "items"
      nextPageToken = objLookup d "nextPageToken"
      ids = map strGet $ V.toList $ arrMap getId (arrFilter isVideo items)
   in do msp ("GOSH", count, length ids)
         return (strGet nextPageToken, ids)
  where objLookup (Object x) field = x HM.! (T.pack field)
        arrLookup (Array a) i = (V.toList a) !! i
        arrMap f (Array a) = fmap f a
        arrFilter f (Array a) = Array $ V.fromList $ filter f (V.toList a)
        strGet (String t) = T.unpack t
        getId item = objLookup (objLookup item "id") "videoId"
        isVideo item = strGet (objLookup (objLookup item "id") "kind") == "youtube#video"
        pageTokenParam (Just t) = t
        pageTokenParam Nothing = ""

search :: String -> Int -> IO [String]
search searchString count = untilDoneM (SomeGetter firstGetter) count
  where --getter :: Maybe String -> Int -> IO ([a], SomeGetter a)
        getter pageToken count = do
          (nextPageToken, ids) <- search' pageToken searchString count
          let nextGetter = getter (Just nextPageToken)
          return (ids, SomeGetter nextGetter)
        firstGetter = getter Nothing

data SomeGetter a = SomeGetter (Int -> IO ([a], SomeGetter a))
untilDoneM :: SomeGetter a -> Int -> IO [a]
untilDoneM getter count = untilDoneM' getter count 0
untilDoneM' :: SomeGetter a -> Int -> Int -> IO [a]
untilDoneM' getter count soFar | soFar >= count = return []
untilDoneM' (SomeGetter getter) count soFar = do
  let toGet = count - soFar
  (some, nextGetter) <- getter toGet
  theRest <- untilDoneM' nextGetter count (soFar + length some)
  return $ some ++ theRest
