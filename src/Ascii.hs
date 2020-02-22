module Ascii
  ( revAsc
  , box
  , boxShowMember
  , gridder
  ) where

import Data.List (intercalate)
import Text.Printf (printf)

revAsc :: String -> String
revAsc s = "\ESC[7m" ++ s ++ "\ESC[0m" 

box :: Bool -> Int -> String
box reverse i =
  let base = "[" ++ (fmt i) ++ "]"
   in if reverse then revAsc base else base
  where fmt i = printf "%4d" i

boxShowMember :: (Int -> Bool) -> Int -> String
boxShowMember isRev i = box (isRev i) i

-- Render a grid of integers, some in reverse video
gridder :: Int -> (Int -> Bool) -> String
gridder n isRev = intercalate "\n" $ map format $ splitUp 10 $ map (boxShowMember isRev) [0..n-1]
  where format xs = intercalate " " xs
        splitUp :: Int -> [a] -> [[a]]
        splitUp n [] = []
        splitUp n xs = take n xs : splitUp n (drop n xs)
