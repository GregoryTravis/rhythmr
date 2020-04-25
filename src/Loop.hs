module Loop
( Loop(..)
, loopFilename
, getHash ) where

import Data.List.Split (splitOn)
import System.FilePath.Posix (dropExtension, takeBaseName)

import Util

newtype Loop = Loop String
  deriving (Eq, Read, Show, Ord)

loopFilename :: Loop -> String
loopFilename (Loop f) = f

getHash :: Loop -> String
getHash (Loop filename) = case (splitOn "-" $ dropExtension filename) of [_, _, _, hash] -> hash
