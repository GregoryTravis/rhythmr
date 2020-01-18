{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Affinity (affinityMain) where

import Control.Concurrent
import Data.List (intercalate, transpose)
import qualified Data.Set as S
import System.Directory (listDirectory)
import System.Random
import Text.Printf (printf)

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
        , currentGroup :: [Int]
        , looper :: Looper }

initState :: Looper -> IO State
initState looper = do
  filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
  sounds <- mapM readSound filenames
  return $ State { sounds = sounds, likes = empty, dislikes = empty, currentGroup = [], looper = looper }

-- TODO maybe function type aliases are not good
-- type KeyboardHandler s = s -> Char -> IO (s, Bool)
keyboardHandler :: KeyboardHandler State
keyboardHandler s 'r' = do
  group <- randomGroup s
  let s' = s { currentGroup = group }
  playCurrent s'
  return (s', False)
keyboardHandler s 'y' = return (judge True s, False)
keyboardHandler s 'n' = return (judge False s, False)
keyboardHandler s 'A' = do
  case acceptable s of [] -> return (s, False)
                       (g:gs) -> do
                                   let s' = s { currentGroup = g }
                                   playCurrent s'
                                   return (s', False)
keyboardHandler s 'S' = do
  playSong s
  return (s, False)

playSong :: State -> IO ()
playSong s = do
  let acc = acceptable s
      accSounds = map (map ((sounds s) !!)) acc
      arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleSoundArrangement loopLengthFrames) ss)) accSounds
  songMix <- renderArrangement arr
  setSound (looper s) songMix
  where dub x = seqArrangement [x, x]

playCurrent :: State -> IO ()
playCurrent s = do
  oneSeven <- readSound "1-7loops/1-7.wav"
  let ss :: [Sound]
      ss = map ((sounds s) !!) (currentGroup s)
      arr :: Arrangement
      arr = parArrangement (map (singleSoundArrangement loopLengthFrames) (oneSeven : ss))
  mix <- renderArrangement arr
  --msp "setting sound"
  setSound (looper s) mix

judge :: Bool -> State -> State
judge isLike s = do
  let setter = \s g -> if isLike then s { likes = g } else s { dislikes = g }
      getter = if isLike then likes else dislikes
      addComponent :: Graph Int -> [Int] -> Graph Int
      addComponent g xs = addMulti g (map (head xs,) (tail xs))
   in (setter s) $ addComponent (getter s) (currentGroup s)

randFromList :: [a] -> IO a
randFromList xs = do
  i <- getStdRandom (randomR (0, length xs - 1))
  return $ xs !! i

randomGroup s = do
  let inUse = nodes (likes s)
      unused = S.toList $ (S.fromList [0..length (sounds s)-1]) `S.difference` inUse
  groupSize <- getStdRandom (randomR (2,4)) :: IO Int
  indices <- mapM (\_ -> randFromList unused) [0..groupSize-1]
  return indices

acceptable :: State -> [[Int]]
acceptable (State { likes, dislikes }) = map S.toList $ components likes
  -- let allPairs = esp [(ca, cb) | ca <- components likes, cb <- components dislikes]
  --  in map S.toList $ map (uncurry S.difference) allPairs
   --in map (map S.toList) $ map (uncurry setDiff) allPairs
  --where setDiff a b = S.fromList a `difference` S.fromList b

displayer :: Displayer State
displayer s = intercalate "\n" lines
  where gridS = grid s
        lines = [gridS, "", currentS, likesS, dislikesS, "", arrS]
        --soundsS = "Sounds: " ++ showList [0..length (sounds s)-1]
        currentS = "Current: " ++ showList (currentGroup s)
        likesS = "Likes: " ++ (showGraphAsComponents $ likes s)
        dislikesS = "Dislikes: " ++ (showGraphAsComponents $ dislikes s)
        --acceptableS = "Acceptable: " ++ show (acceptable s)
        arrS = showArr (acceptable s)
        showList xs = intercalate " " (map show xs)

rev :: String -> String
rev s = "\ESC[7m" ++ s ++ "\ESC[0m" 

-- box :: [Int] -> Int -> String
-- box group i = rv i ("[" ++ (fmt i) ++ "]")
--   where fmt i = printf "%4d" i
--         rv :: Int -> String -> String
--         rv i s = if elem i group then rev s else s
box :: Bool -> Int -> String
box reverse i =
  let base = "[" ++ (fmt i) ++ "]"
   in if reverse then rev base else base
  where fmt i = printf "%4d" i

showArr :: [[Int]] -> String
showArr ises = gridize $ transpose $ map (\is -> (map (box False) is)) ises
  where gridize xses = intercalate "\n" (map (\xs -> (intercalate " " xs)) xses)

boxShowMember :: [Int] -> Int -> String
boxShowMember group i = box (elem i group) i

grid :: State -> String
grid (State { sounds, currentGroup }) = intercalate "\n" $ map format $ splitUp 10 $ map (boxShowMember currentGroup) [0..length sounds - 1]
  where format xs = intercalate " " xs
        splitUp :: Int -> [a] -> [[a]]
        splitUp n [] = []
        splitUp n xs = take n xs : splitUp n (drop n xs)

affinityMain :: Int -> IO ()
affinityMain seed = do
  withLooper $ \looper -> do
                    s <- initState looper
                    runEditor (editor s keyboardHandler displayer)
