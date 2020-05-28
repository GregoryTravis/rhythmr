module ISO8601Duration (parseDuration) where

import qualified Data.Map as M

import Util

parseDuration :: String -> Int
parseDuration "P0D" = 0
parseDuration ds = sum (map calc (zipUp $ removePT $ (predSplit isDigit ds)))
  where zipUp (a : b : xs) = (a, b) : zipUp xs
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

