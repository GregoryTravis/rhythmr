{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Stow
( stowMain ) where

import Util

class Stowable a where
  stow :: a -> String

instance Show a => Stowable a where
  stow = show

data W = W { ints :: [Int]
           , string :: String }
  deriving Show

stowMain = do
  let w = W { ints = [1, 2], string = "asdf" }
  msp $ stow w
  msp "hi"
