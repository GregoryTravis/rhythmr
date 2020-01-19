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

editorLogLength = 10

addClick :: Maybe String
addClick = Nothing
--addClick = Just "looper/1-7.wav"

data State =
  State { sounds :: [Sound]
        , likes :: Graph Int
        , dislikes :: S.Set [Int]
        , currentGroup :: [Int]
        , looper :: Looper
        , editorLog :: [String]
        , stack :: [[Int]] }

initState :: Looper -> IO State
initState looper = do
  filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
  sounds <- mapM readSound filenames
  return $ State { sounds = sounds, likes = empty, dislikes = S.empty, currentGroup = [], looper = looper,
                   editorLog = ["Welcome to autobeat"], stack = [] }

khsuc :: State -> IO (State, Bool)
khsuc s = return (s, False)

-- TODO maybe function type aliases are not good
-- type KeyboardHandler s = s -> Char -> IO (s, Bool)
keyboardHandler :: KeyboardHandler State
keyboardHandler s 'r' = do
  group <- randomGroup s
  let s' = s { currentGroup = group }
  playCurrent s'
  return (s', False)
keyboardHandler s 'y' = return (like s, False)
keyboardHandler s 'n' = return (dislike s, False)
keyboardHandler s 'A' = do
  case acceptable s of [] -> return (s, False)
                       (g:gs) -> do
                                   let s' = s { currentGroup = g }
                                   playCurrent s'
                                   return (s', False)
keyboardHandler s 'S' = do
  playSong s
  return (s, False)
keyboardHandler s '\ESC' = return (s, True)
keyboardHandler s 'p' = do
  let s' = nextFromStack $ pushCurrentGroup s
  playCurrent s'
  return (s', False)
  --khsuc $ (nextFromStack . pushCurrentGroup) s
keyboardHandler s ' ' = do
  let s' = nextFromStack s
  playCurrent s'
  return (s', False)
  --khsuc $ nextFromStack s
keyboardHandler s key = return (s', False)
  where s' = edlog s ("?? " ++ (show key))

pushCurrentGroup :: State -> State
pushCurrentGroup s = s { stack = map p2l $ allPairs (currentGroup s) }
  where p2l (x, y) = [x, y]

nextFromStack :: State -> State
nextFromStack s | (stack s) /= [] = s { currentGroup = g, stack = gs }
                | otherwise = s
  where (g:gs) = stack s

allPairs (x:xs) = (zip (repeat x) xs) ++ allPairs xs
allPairs [] = []

edlog st msg = st { editorLog = take editorLogLength (msg : editorLog st) }

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
  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readSound filename
                                 Nothing -> return []
  let ss :: [Sound]
      ss = map ((sounds s) !!) (currentGroup s)
      arr :: Arrangement
      arr = parArrangement (map (singleSoundArrangement loopLengthFrames) (clickTrack ++ ss))
  mix <- renderArrangement arr
  --msp "setting sound"
  setSound (looper s) mix

like :: State -> State
like s = s { likes = addAll (likes s) (currentGroup s) }
dislike :: State -> State
dislike s = s { dislikes = S.insert (currentGroup s) (dislikes s) }

randFromList :: [a] -> IO a
randFromList xs = do
  i <- getStdRandom (randomR (0, length xs - 1))
  return $ xs !! i

-- This one only picks from the set of loops that aren't part of a like
-- randomGroup s = do
--   let inUse = nodes (likes s)
--       unused = S.toList $ (S.fromList [0..length (sounds s)-1]) `S.difference` inUse
--   groupSize <- getStdRandom (randomR (2,4)) :: IO Int
--   indices <- mapM (\_ -> randFromList unused) [0..groupSize-1]
--   return indices

randomGroup :: State -> IO [Int]
randomGroup s = do
  let soundIndices = [0..length (sounds s)-1]
  groupSize <- getStdRandom (randomR (2,4)) :: IO Int
  indices <- mapM (\_ -> randFromList soundIndices) [0..groupSize-1]
  return indices

acceptable :: State -> [[Int]]
acceptable (State { likes }) = map S.toList $ components likes
  -- let allPairs = esp [(ca, cb) | ca <- components likes, cb <- components dislikes]
  --  in map S.toList $ map (uncurry S.difference) allPairs
   --in map (map S.toList) $ map (uncurry setDiff) allPairs
  --where setDiff a b = S.fromList a `difference` S.fromList b

displayer :: Displayer State
displayer s = intercalate "\n" lines
  where gridS = grid s
        lines = [gridS, "", currentS, likesS, dislikesS, stackS, "", arrS, logS]
        --soundsS = "Sounds: " ++ showList [0..length (sounds s)-1]
        currentS = "Current: " ++ showList (currentGroup s)
        likesS = "Likes: " ++ (showGraphAsComponents $ likes s)
        dislikesS = "Dislikes: " ++ showList (map showList (S.toList (dislikes s)))
        stackS = "Stack: " ++ showList (map showList (stack s))
        --acceptableS = "Acceptable: " ++ show (acceptable s)
        arrS = showArr (acceptable s)
        logS = bar ++ "\n" ++ (intercalate "\n" (extend (reverse $ editorLog s))) ++ "\n" ++ bar
          where extend :: [String] -> [String]
                extend lines = take editorLogLength $ lines ++ (repeat "")
        showList xs = intercalate " " (map show xs)
        bar = "======================"

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
