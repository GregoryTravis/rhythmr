module Aubio
( aubioTrack ) where

import External

parseAubioOutput :: String -> [[String]]
parseAubioOutput s = map words (lines s)

aubioTrack :: String -> IO [Double]
aubioTrack file = do
  s <- readFromProc "aubiotrack" [file]
  return $ map (\row -> case row of [fs] -> (read fs) :: Double) (parseAubioOutput s)
