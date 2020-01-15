{-# LANGUAGE TupleSections #-}

module Affinity (affinityMain) where

import Control.Concurrent
import Data.List (intercalate)
import System.Directory (listDirectory)
import System.Random

import Arrangement
import Constants
import Graph
import Looper
import Sound
import TUI
import Util

data State =
  State { sounds :: [Sound]
        , likes :: Graph Int
        , dislikes :: Graph Int
        , currentGroup :: [Int] }
emptyState = State { sounds = [], likes = empty, dislikes = empty, currentGroup = [] }

initState :: IO State
initState = do
  filenames <- fmap (map ("loops/" ++)) $ fmap (take 8) $ listDirectory "loops"
  sounds <- mapM readSound filenames
  return $ emptyState { sounds = sounds }

-- TODO maybe function type aliases are not good
-- type KeyboardHandler s = s -> Char -> IO (s, Bool)
keyboardHandler :: KeyboardHandler State
keyboardHandler s 'r' = do
  group <- randomGroup s
  let s' = s { currentGroup = group }
  return (s', False)
keyboardHandler s 'y' = return (judge True s, False)
keyboardHandler s 'n' = return (judge False s, False)

judge :: Bool -> State -> State
judge isLike s = do
  let setter = \s g -> if isLike then s { likes = g } else s { dislikes = g }
      getter = if isLike then likes else dislikes
      addComponent :: Graph Int -> [Int] -> Graph Int
      addComponent g xs = addMulti g (map (head xs,) (tail xs))
   in (setter s) $ addComponent (getter s) (currentGroup s)

randomGroup s = do
  let numLoops = length $ sounds s
  groupSize <- getStdRandom (randomR (2,4)) :: IO Int
  --indices <- fmap (take numLoops) $ getStdRandom (randomRs (0, numLoops-1)) :: IO [Int]
  indices <- mapM (\_ -> getStdRandom (randomR (0, numLoops-1))) [0..groupSize-1]
  return indices

displayer :: Displayer State
displayer s = intercalate "\n" lines
  where lines = [currentS, soundsS, likesS, dislikesS]
        soundsS = "Sounds: " ++ showList [0..length (sounds s)-1]
        currentS = "Current: " ++ showList (currentGroup s)
        likesS = "Likes: " ++ (showGraphAsComponents $ likes s)
        dislikesS = "Dislikes: " ++ (showGraphAsComponents $ dislikes s)
        showList xs = intercalate " " (map show xs)

affinityMain :: Int -> IO ()
affinityMain seed = do
  s <- initState
  runEditor (editor s keyboardHandler displayer)

__affinityMain :: Int -> IO ()
__affinityMain seed = do
  let g :: Graph Int
      --g = add (add (add empty 3 4) 1 2) 1 3
      g = add (add empty 3 4) 1 2
  msp g
  msp $ showComponents $ components g
  sound <- readSound "aloop.wav"
  sound2 <- readSound "bloop.wav"
  sound3 <- readSound "cloop.wav"
  withLooper $ \looper -> do
    sendCommand looper $ Play sound
    threadDelay $ 5 * 1000000
    sendCommand looper $ Play sound3
    sendCommand looper $ Play sound2
    threadDelay $ 3 * 1000000
    sendCommand looper $ Stop
  msp "aff hi"

_affinityMain seed = do
  let rand = mkStdGen seed
  loopFilenames <- fmap (map ("loops/"++)) $ listDirectory "loops"
  let someLoopFilenames = map (loopFilenames !!) $ take 4 $ randomRs (0, length loopFilenames - 1) rand
  msp someLoopFilenames
  loops <- mapM readSound someLoopFilenames
  msp loops
  let arrangement :: Arrangement
      arrangement = rep 4 $ parArrangement (map (singleSoundArrangement loopLengthFrames) loops)
  msp arrangement
  stack <- renderArrangement arrangement
  msp "aff hi"
  writeSound "group.wav" stack
  where rep n arr = seqArrangement (take n $ repeat arr)
