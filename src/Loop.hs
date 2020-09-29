{-# LANGUAGE DeriveGeneric #-}

module Loop
( Loop(..)
, loopFilename
, getHash
, getSourceTrackHash
, getSourceTrackName ) where

import Data.Binary
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import System.FilePath.Posix (dropExtension, takeBaseName)
import System.IO.Unsafe (unsafePerformIO)

import Memoize
import Util

newtype Loop = Loop String
  deriving (Eq, Read, Show, Ord, Generic)

instance Binary Loop

loopFilename :: Loop -> String
loopFilename (Loop f) = f

parseFilename :: Loop -> [String]
parseFilename (Loop filename) = splitOn "-" $ takeBaseName $ dropExtension filename

getHash' :: Loop -> String
getHash' = (!!! 3) . parseFilename
getHash = unsafePerformIO (memoizePure getHash')

getSourceTrackHash :: Loop -> String
getSourceTrackHash = (!!! 2) . parseFilename

getSourceTrackName :: Loop -> String
getSourceTrackName loop =
  case parseFilename loop of [_, x, y, _] -> x ++ "-" ++  y
