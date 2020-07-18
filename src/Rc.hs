{-# LANGUAGE DeriveGeneric #-}

module Rc (rc, Rc(..)) where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.Maybe
import GHC.Generics
import System.IO
import System.IO.Unsafe (unsafePerformIO)

data Rc = Rc { cacheDownloads :: Bool } deriving (Generic, Show)

instance FromJSON Rc

readRc :: IO Rc
readRc = do
  rcString <- BLU.fromString <$> readFile ".rhythmrc"
  return $ fromJust $ decode rcString

rc = unsafePerformIO readRc
