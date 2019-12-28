module Arrangement
( Arrangement(..)
, getArrangementElements
, mixdown ) where

import Data.List

import Sound

data Arrangement a = Elem a | Par [Arrangement a] | Seq [Arrangement a]

instance Functor Arrangement where
  fmap f (Elem e) = Elem (f e)
  fmap f (Par es) = Par (map (fmap f) es)
  fmap f (Seq es) = Seq (map (fmap f) es)

getArrangementElements :: Ord a => Arrangement a -> [a]
getArrangementElements seq = sort $ nub $ get' seq
  where get' (Elem a) = [a]
        get' (Par seqs) = concat (map get' seqs)
        get' (Seq seqs) = concat (map get' seqs)

mixdown :: Arrangement Sound -> Sound
mixdown seq = normalize (mixdown' seq)
  where mixdown' (Elem sound) = sound
        mixdown' (Par mixes) = mixSounds (map mixdown' mixes)
        mixdown' (Seq mixes) = appendSounds (map mixdown' mixes)
