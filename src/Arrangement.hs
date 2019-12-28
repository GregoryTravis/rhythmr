module Arrangement
( TiledArrangement(..)
, getTiledArrangementElements
, mixdown ) where

import Data.List

import Sound

data TiledArrangement a = Elem a | Par [TiledArrangement a] | Seq [TiledArrangement a]

instance Functor TiledArrangement where
  fmap f (Elem e) = Elem (f e)
  fmap f (Par es) = Par (map (fmap f) es)
  fmap f (Seq es) = Seq (map (fmap f) es)

getTiledArrangementElements :: Ord a => TiledArrangement a -> [a]
getTiledArrangementElements seq = sort $ nub $ get' seq
  where get' (Elem a) = [a]
        get' (Par seqs) = concat (map get' seqs)
        get' (Seq seqs) = concat (map get' seqs)

mixdown :: TiledArrangement Sound -> Sound
mixdown seq = normalize (mixdown' seq)
  where mixdown' (Elem sound) = sound
        mixdown' (Par mixes) = mixSounds (map mixdown' mixes)
        mixdown' (Seq mixes) = appendSounds (map mixdown' mixes)
