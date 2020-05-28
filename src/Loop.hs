module Loop
( Loop(..)
, loopFilename
, getHash
, getSourceTrackHash ) where

import Data.List.Split (splitOn)
import System.FilePath.Posix (dropExtension, takeBaseName)

import Util

newtype Loop = Loop String
  deriving (Eq, Read, Show, Ord)

loopFilename :: Loop -> String
loopFilename (Loop f) = f

parseFilename :: Loop -> [String]
parseFilename (Loop filename) = splitOn "-" $ takeBaseName $ dropExtension filename

getHash :: Loop -> String
getHash = (!! 3) . parseFilename

getSourceTrackHash :: Loop -> String
getSourceTrackHash = (!! 2) . parseFilename
