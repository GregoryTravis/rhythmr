{-# LANGUAGE DeriveGeneric #-}

module Rc (rc, Rc(..)) where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.Maybe
import GHC.Generics
import System.Directory (doesFileExist)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

rcFile = ".rhythmrc"

data Rc = Rc { cacheDownloads :: Bool } deriving (Generic, Show)

defaultRc :: Rc
defaultRc = Rc { cacheDownloads = False }

instance FromJSON Rc

readRc :: IO Rc
readRc = do
  b <- doesFileExist rcFile
  if b then do rcString <- BLU.fromString <$> readFile rcFile
               return $ fromJust $ decode rcString
       else return defaultRc

rc = unsafePerformIO readRc
