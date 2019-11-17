{-# LANGUAGE BlockArguments #-}

module Sequence
( Sequence(..)
, getSequenceElements
, renderSequence
) where

import Control.Monad (replicateM)
import Data.List
import qualified Data.Set as S
import System.Random

import Aubio
import Resample
import Sound
import Util

data Sequence = Sequence [[Int]]

data ProcessedFile = ProcessedFile Sound [Double]

bmp = 120
meter = 4
loopLengthSeconds = (60.0 / fromInteger bmp) * meter
standardSR = 44100
loopLengthFrames = floor $ (fromInteger standardSR) * loopLengthSeconds

getSequenceElements :: Sequence -> [Int]
getSequenceElements (Sequence measures) =
  sort $ S.toList $ S.unions $ map S.fromList measures

processFile :: String -> IO ProcessedFile
processFile filename = do
  sound <- readSound filename
  track <- aubioTrack filename
  return $ ProcessedFile sound track

renderSequence :: Sequence -> [String] -> IO ()
renderSequence sequence filenames = do
  let numLoops = length (getSequenceElements sequence)
  pfs <- mapM processFile filenames
  loops <- getRandomLoops numLoops pfs
  resampledLoops <- mapM (resampleSound loopLengthFrames) loops
  flip mapM (zip [0..] resampledLoops) $ \(i, loop) -> do
    writeSound ("loop" ++ (show i) ++ ".wav") loop
  return ()

getRandomLoops :: Int -> [ProcessedFile] -> IO [Sound]
getRandomLoops numLoops pfs = do
  let loopSig = 4
  setStdGen $ mkStdGen 23
  replicateM numLoops do
    pfNum <- getStdRandom (randomR (0, length pfs - 1))
    msp $ "pf " ++ show pfNum
    let ProcessedFile sound track = pfs !! pfNum
    startTick <- getStdRandom (randomR (0, length track - loopSig))
    let ticks = drop startTick track
    let start = ticks !! 0
    let end = ticks !! loopSig
    msp $ ("snip", startTick, start, end)
    return $ snip start end sound
