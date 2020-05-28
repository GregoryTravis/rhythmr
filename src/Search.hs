module Search (search, searchNoPaging) where

--import Data.Aeson (Value(Array, String, Object))
import qualified Data.Map as M
--import Data.Time.ISO8601.Duration
import qualified Data.Vector as V

import AesonHelpers
import External
import ISO8601Duration
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

search' :: Maybe String -> String -> Int -> IO (Maybe String, [String])
search' pageToken searchString count = do
  Just d <- cachedJsonCommand "python" ["get-mp3s.py", searchString, show count, pageTokenParam pageToken]
  let search = objLookup d "search"
      videos = objLookup d "videos"
      items = objLookup search "items"
      nextPageToken = objLookupMaybe search "nextPageToken"
      durationMap = getDurationMap videos
      ids = filter (durationIsOk durationMap) $ map strGet $ V.toList $ arrMap getId (arrFilter isVideo items)
   in do msp $ M.map parseDuration $ esp durationMap
         return (nextPageToken, ids)
  where getId item = objLookups item ["id", "videoId"]
        isVideo item = strGet (objLookups item ["id", "kind"]) == "youtube#video"
        pageTokenParam (Just t) = t
        pageTokenParam Nothing = ""
        getIdAndDuration item = (id, duration)
            where id = strGet $ objLookup item "id"
                  duration = strGet $ objLookups item ["contentDetails", "duration"]
        getDurationMap videos = M.fromList $ map getIdAndDuration $ map one $ map (flip objLookup "items") $ V.toList $ arrGet videos
        durationIsOk durationMap id = maybe False durationValueOk $ durationMap M.!? id
        durationValueOk durationString =
          let n = parseDuration durationString
           in n > 0 && n < (8*60)

search :: String -> Int -> IO [String]
search searchString count = untilDoneM (SomeGetter firstGetter) count
  where --getter :: Maybe String -> Int -> IO ([a], SomeGetter a)
        getter pageToken count = do
          (nextPageToken, ids) <- search' pageToken searchString count
          let nextGetter = getter nextPageToken
          msp "search return"
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
