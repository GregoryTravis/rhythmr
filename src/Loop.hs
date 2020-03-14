module Loop
( Loop(..)
, loopFilename ) where

newtype Loop = Loop String
  deriving (Eq, Read, Show, Ord)

loopFilename :: Loop -> String
loopFilename (Loop f) = f
