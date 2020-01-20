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
        , likes :: S.Set [Int]
        , dislikes :: S.Set [Int]
        , currentGroup :: [Int]
        , looper :: Looper
        , editorLog :: [String]
        , stack :: [[Int]] }

-- Suitable for persisting
data StateRep = 
  StateRep { repLikes :: S.Set [Int]
           , repDislikes :: S.Set [Int] }
  deriving (Read, Show)

emptyStateRep = StateRep { repLikes = S.empty, repDislikes = S.empty }

toRep :: State -> StateRep
toRep (State { likes, dislikes }) = (StateRep { repLikes = likes, repDislikes = dislikes })

-- We do them as a group since we only want to load the sounds once for all of them
fromReps :: Looper -> [StateRep] -> IO [State]
fromReps looper reps = do
  sounds <- (loadLoops :: IO [Sound])
  return $ map (fromRep sounds) reps
  where fromRep :: [Sound] -> StateRep -> State
        fromRep sounds (StateRep { repLikes, repDislikes }) =
          State { sounds, likes = repLikes, dislikes = repDislikes, currentGroup = [], looper,
                  editorLog = ["Welcome to autobeat"], stack = [] }

--loader :: Loader State [StateRep]
loader :: State -> [StateRep] -> IO [State]
loader currentState reps = fromReps (looper currentState) reps

saver :: [State] -> [StateRep]
saver = map toRep

-- This is not used; it is required so that KHResults can be compared
instance Eq State where
  _ == _ = undefined

loadLoops :: IO [Sound]
loadLoops = do
  filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
  mapM readSound filenames

initState :: Looper -> IO State
initState looper = do
  [s] <- fromReps looper [emptyStateRep]
  return s

-- TODO maybe function type aliases are not good
keyboardHandler :: KeyboardHandler State
keyboardHandler s 'r' = do
  group <- randomGroup s
  let s' = s { currentGroup = group }
  --playCurrent s'
  return $ SetState s'
keyboardHandler s 'y' = return (SetState $ like s)
keyboardHandler s 'n' = return (SetState $ dislike s)
keyboardHandler s 'A' = do
  case acceptable s of [] -> return DoNothing
                       (g:gs) -> do
                                   let s' = s { currentGroup = g }
                                   return (SetState s')
keyboardHandler s 'S' = do
  playSong s
  return (SetState s)
keyboardHandler s '\ESC' = return Quit
keyboardHandler s 'p' = do
  let s' = nextFromStack $ pushCurrentGroup s
  --msp ("eh", currentGroup s, stack s)
  --msp ("eh", currentGroup s', stack s')
  return (SetState s')
keyboardHandler s ' ' = do
  s' <- if stack s == []
             then return $ edlog s "Stack empty, yo"
             else return $ nextFromStack s
  --let s' = nextFromStack s
  return (SetState s')
keyboardHandler s 'u' = return Undo
keyboardHandler s '\DC2' = return Redo
keyboardHandler s 's' = return $ Save "history.txt"
keyboardHandler s 'L' = return $ Load "history.txt"
keyboardHandler s key = return (SetState s')
  where s' = edlog s ("?? " ++ (show key))

respondToStateChange :: State -> State -> IO ()
respondToStateChange s s' = do
  if currentGroup s' /= currentGroup s && currentGroup s' /= []
      then playCurrent s'
      else return ()

keyboardHandlerWrapper :: KeyboardHandlerWrapper State
keyboardHandlerWrapper kh s k = do
  khr <- kh s k
  case khr of SetState s' -> respondToStateChange s s'
              _ -> return ()
  -- case khr of SetState s' -> if currentGroup s' /= currentGroup s
  --                               then playCurrent s'
  --                               else return ()
  --             _ -> return ()
  return khr

pushCurrentGroup :: State -> State
pushCurrentGroup s = s { stack = map p2l $ allPairs (currentGroup s) }
  where p2l (x, y) = [x, y]

nextFromStack :: State -> State
nextFromStack s | (stack s) /= [] = s { currentGroup = g, stack = gs }
                | otherwise = s
  where (g:gs) = stack s

allPairs (x:xs) = (zip (repeat x) xs) ++ allPairs xs
allPairs [] = []

edlog :: State -> String -> State
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
like s = s { likes = S.insert (currentGroup s) (likes s) }
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
acceptable = (map S.toList) . components . fromComponents . S.toList . likes

displayer :: Displayer State
displayer s = intercalate "\n" lines
  where lines = [gridS, bar, currentS, likesS, dislikesS, stackS, bar, affS, logS]
        gridS = grid s
        currentS = "Current: " ++ showList (currentGroup s)
        likesS = "Likes: " ++ showList (map showList (S.toList (likes s)))
        dislikesS = "Dislikes: " ++ showList (map showList (S.toList (dislikes s)))
        stackS = "Stack: " ++ showList (map showList (stack s))
        affS = "Affinities: " ++ intercalate "\n" (map show (acceptable s))
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
                    runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
