module Ascii
  ( revAsc
  , box
  , boxShowMember
  ) where

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
