module Loop
( Loop(..)
, loopFilename
, getHash ) where

newtype Loop = Loop String
  deriving (Eq, Read, Show, Ord)

loopFilename :: Loop -> String
loopFilename (Loop f) = f

getHash :: Loop -> String
getHash (Loop filename) = take (length filename - 15) $ drop 11 filename
