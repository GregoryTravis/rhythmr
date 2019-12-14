{-# LANGUAGE BlockArguments #-}

module Sequence
( Sequence(..)
, getSequenceElements
, renderSequence
) where

import Control.Monad (replicateM)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random

import Aubio
import Resample
import Sound
import Util

data Sequence a = Elem a | Par [Sequence a] | Seq [Sequence a]

--mapSequence :: (a -> b) -> Sequence a -> Sequence b
instance Functor Sequence where
  fmap f (Elem e) = Elem (f e)
  fmap f (Par es) = Par (map (fmap f) es)
  fmap f (Seq es) = Seq (map (fmap f) es)

data ProcessedFile = ProcessedFile Sound [Int] deriving Show

bmp = 120
meter = 4
loopLengthSeconds = (60.0 / fromInteger bmp) * meter
standardSR = 44100
loopLengthFrames = floor $ (fromInteger standardSR) * loopLengthSeconds

getSequenceElements :: Ord a => Sequence a -> [a]
getSequenceElements seq = sort $ nub $ get' seq
  where get' (Elem a) = [a]
        get' (Par seqs) = concat (map get' seqs)
        get' (Seq seqs) = concat (map get' seqs)

processFile :: String -> IO ProcessedFile
processFile filename = do
  sound <- readSound filename
  track <- barBeat filename
  return $ ProcessedFile sound track

renderSequence :: Sequence Int -> [String] -> Int -> IO ()
renderSequence sequence filenames seed = do
  let numLoops = length (getSequenceElements sequence)
  pfs <- mapM processFile filenames
  loops <- getRandomLoops numLoops pfs seed
  resampledLoops <- mapM (resampleSound loopLengthFrames) loops
  let intToSound = M.fromList (zip (getSequenceElements sequence) resampledLoops)
  flip mapM (zip [0..] resampledLoops) $ \(i, loop) -> do
    writeSound ("loop-" ++ (show i) ++ "-" ++ (show seed) ++ ".wav") loop
  let loopSequence = fmap (intToSound M.!) sequence
  let song = mixdown loopSequence
  writeSound ("song-" ++ (show seed) ++ ".wav") song
  return ()

--showDiffs :: [Int] -> IO ()
showDiffs ns = do
  let diffs = map (uncurry (-)) $ zip ns (0:ns)
  mapM (putStrLn . show) (zip ns diffs)

getRandomLoops :: Int -> [ProcessedFile] -> Int -> IO [Sound]
getRandomLoops numLoops pfs seed = do
  let loopSig = 1
  setStdGen $ mkStdGen seed
  replicateM numLoops do
    pfNum <- getStdRandom (randomR (0, length pfs - 1))
    msp $ "pf " ++ show pfNum
    let ProcessedFile sound track = pfs !! pfNum
    startTick <- getStdRandom (randomR (0, length track - loopSig))
    let ticks = take (loopSig + 1) $ drop startTick track
    let start = ticks !! 0
    let end = ticks !! loopSig
    msp ("snip", startTick, start, end)
    return $ snip start end sound

mixdown :: Sequence Sound -> Sound
mixdown seq = normalize (mixdown' seq)
  where mixdown' (Elem sound) = sound
        mixdown' (Par mixes) = mixSounds (map mixdown mixes)
        mixdown' (Seq mixes) = appendSounds (map mixdown mixes)
