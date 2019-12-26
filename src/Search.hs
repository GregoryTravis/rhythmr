module Search (search, searchNoPaging) where

import Data.Aeson (Value(Array, String, Object))
import Data.ByteString.UTF8 as BSU (fromString, toString)
import Data.List (groupBy)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
--import Data.Time.ISO8601.Duration
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

search' :: Maybe String -> String -> Int -> IO (Maybe String, [String])
search' pageToken searchString count = do
  msp ("hey", pageToken, searchString, count)
  Just d <- cachedJsonCommand "python" ["get-mp3s.py", searchString, show count, pageTokenParam pageToken]
  msp $ "has nextPageToken " ++ show (objHas d "nextPageToken")
  let search = objLookup d "search"
      videos = objLookup d "videos"
      items = objLookup search "items"
      nextPageToken = objLookupMaybe d "nextPageToken"
      durationMap = getDurationMap videos
      ids = filter (durationIsOk durationMap) $ map strGet $ V.toList $ arrMap getId (arrFilter isVideo items)
   in do --msp ("GOSH", count, length ids)
         --msp durationMap
         --msp $ M.map parseDuration durationMap
         --msp "ok"
         return (nextPageToken, ids)
         --return ("", ids)
  where objLookup (Object x) field = x HM.! (T.pack field)
        objLookup x field = undefined
        objLookupMaybe (Object x) field = fmap strGet $ HM.lookup (T.pack field) x
        objHas (Object x) field = HM.member (T.pack field) x
        arrLookup (Array a) i = (V.toList a) !! i
        arrGet (Array a) = a
        arrMap f (Array a) = fmap f a
        arrFilter f (Array a) = Array $ V.fromList $ filter f (V.toList a)
        strGet (String t) = T.unpack t
        one xs = let l = V.toList (arrGet xs)
                  in assertM "one" (length l == 1) (l !! 0)
        getId item = objLookup (objLookup item "id") "videoId"
        isVideo item = strGet (objLookup (objLookup item "id") "kind") == "youtube#video"
        pageTokenParam (Just t) = t
        pageTokenParam Nothing = ""
        getIdAndDuration item = (id, duration)
            where id = strGet $ objLookup item "id"
                  duration = strGet $ objLookup (objLookup item "contentDetails") "duration"
        getDurationMap :: Value -> M.Map String String
        --getDurationMap videos = M.fromList $ map getIdAndDuration (V.toList $ arrGet (objLookup videos "items"))
        getDurationMap videos = M.fromList $ map getIdAndDuration $ map one $ map (\v -> objLookup v "items") $ V.toList $ arrGet videos
        durationIsOk durationMap id = case (durationMap M.!? id) of Nothing -> False
                                                                    (Just ds) -> durationIsNotTooLong ds
        durationIsNotTooLong ds = (parseDuration ds) < (8 * 60)
        parseDuration ds = sum (map calc (zipUp $ removePT $ (predSplit isDigit ds)))
        zipUp (a : b : xs) = (a, b) : zipUp xs
        zipUp [] = []
        removePT ("PT" : rest) = rest
        calc :: (String, String) -> Int
        calc (n, c) = assertM m b (read n :: Int) * (durationLetter M.! (c !! 0))
          where m = "iso8601 duration thing isn't 1 char, or missing " ++ c
                b = length c == 1 && M.member (c !! 0) durationLetter
        durationLetter :: M.Map Char Int
        durationLetter = M.fromList [
          ('S', 1),
          ('M', 60),
          ('H', 3600) ]
        isDigit c = c >= '0' && c <= '9'
        -- parseISO8601Duration s = parseDuration bs
        --   where bs = BSU.fromString s

predSplit p xs = groupBy same xs
  where same a b = p a == p b

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
