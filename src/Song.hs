{-# LANGUAGE BlockArguments #-}

module Song
( renderSong
) where

import Control.Monad (replicateM)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Random

import Arrangement
import Aubio
import Resample
import Sound
import Util

data ProcessedFile = ProcessedFile Sound [Int] deriving Show

bmp = 120
meter = 4
loopLengthSeconds = (60.0 / fromInteger bmp) * meter
standardSR = 44100
loopLengthFrames = floor $ (fromInteger standardSR) * loopLengthSeconds

processFile :: String -> IO ProcessedFile
processFile filename = do
  sound <- readSound filename
  track <- barBeat filename
  return $ ProcessedFile sound track

renderSong :: [[Int]] -> [String] -> Int -> IO ()
renderSong ises filenames seed = do
  let loopInts = sort $ nub $ concat ises
      numLoops = length loopInts
  pfs <- mapM processFile filenames
  loops <- getRandomLoops numLoops pfs seed
  let intToSound = M.fromList (zip loopInts loops)
  -- flip mapM (zip [0..] resampledLoops) $ \(i, loop) -> do
  --   writeSound ("loop-" ++ (show i) ++ "-" ++ (show seed) ++ ".wav") loop
  let loopArrangement :: [[Sound]]
      loopArrangement = fmap (fmap (intToSound M.!)) ises
  msp loopArrangement
  let arrangement :: Arrangement
      arrangement = seqArrangement $ map (\pas -> parArrangement (map (singleSoundArrangement loopLengthFrames) pas)) loopArrangement
  msp arrangement
  song <- renderArrangement arrangement
  writeSound ("song-" ++ (show seed) ++ "-new.wav") song
  return ()

--showDiffs :: [Int] -> IO ()
showDiffs ns = do
  let diffs = map (uncurry (-)) $ zip ns (0:ns)
  mapM (putStrLn . show) (zip ns diffs)

brep rand = do
  getStdRandom rand

brepDebug rand = do
  sg <- getStdGen
  msp $ "RND before " ++ (show sg)
  x <- getStdRandom rand
  msp $ "RND n " ++ (show x)
  sg' <- getStdGen
  msp $ "RND after " ++ (show sg')
  return x

getRandomLoops :: Int -> [ProcessedFile] -> Int -> IO [Sound]
getRandomLoops numLoops pfs seed = do
  let loopSig = 1
  setStdGen $ mkStdGen seed
  replicateM numLoops do
    msp $ "num pfs " ++ (show $ length pfs)
    pfNum <- brep (randomR (0, length pfs - 1))
    msp $ "pf " ++ show pfNum
    let ProcessedFile sound track = pfs !! pfNum
    startTick <- brep (randomR (0, length track - loopSig - 1))
    let ticks = take (loopSig + 1) $ drop startTick track
    msp "ticks"
    msp ticks
    msp (length track)
    msp startTick
    let start = ticks !! 0
    let end = ticks !! loopSig
    msp ("snip", startTick, start, end)
    return $ snip start end sound
