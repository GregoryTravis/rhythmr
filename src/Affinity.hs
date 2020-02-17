{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Affinity (affinityMain) where

import Control.Concurrent
import Data.List (intercalate, transpose, sortOn)
import qualified Data.Set as S
import Linear
import System.Directory (listDirectory)
import System.Random
import Text.Printf (printf)

import Arrangement
import Constants
import Dice
import FX
import Gui
import Graph
import Looper
import Score
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
keyboardHandler s 's' = return $ Save "history.ab"
keyboardHandler s 'L' = return $ Load "history.ab"
keyboardHandler s 'C' = let s' = (combineAffinities s) in return $ (SetState s')
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

combineAffinities :: State -> State
combineAffinities s =
  case acceptable s of (a : b : _) -> nextFromStack $ replaceStack s (combos a b)
                       _ -> s
  where combos :: [Int] -> [Int] -> [[Int]]
        combos xs ys = [xs' ++ ys' | xs' <- clump 2 xs, ys' <- clump 2 ys]

clump :: Int -> [a] -> [[a]]
clump n [] = []
clump n xs = (take n xs) : (clump n (drop n xs))

replaceStack :: State -> [[Int]] -> State
replaceStack s stack_ = s { stack = stack_ }

pushStack :: State -> [Int] -> State
pushStack s x = s { stack = x : stack s }
pushStackN :: State -> [[Int]] -> State
pushStackN s (x : xs) = pushStack (pushStackN s xs) x
pushStackN s [] = s

allPairs (x:xs) = (zip (repeat x) xs) ++ allPairs xs
allPairs [] = []

edlog :: State -> String -> State
edlog st msg = st { editorLog = take editorLogLength (msg : editorLog st) }

arrVolume :: Arrangement -> IO Float
arrVolume arr = do
  sound <- renderArrangement arr
  return (volume sound)

shv arr = do
  v <- arrVolume arr
  msp v

playSong :: State -> IO ()
playSong s = do
  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readSound filename
                                 Nothing -> return []
  let clickTrackArr = parArrangement (map (singleSoundArrangement loopLengthFrames) clickTrack)
  let sis = currentGroup s -- should be affinity group or something / 68
      someSounds = map ((sounds s) !!) sis
  let score = Score [[Measure 0 NoFX],
                     [Measure 0 (Reverb 85)],
                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40)],
                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40), Measure 2 MCompand],
                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40), Measure 2 MCompand, Measure 3 revReverb]]
      revReverb = FXs [Reverse, Reverb 85, Reverse]
  --arr <- renderScore score someSounds
  let sound = (someSounds !! 0)
      snd = singleSoundArrangement loopLengthFrames sound
      doub = double (singleSoundArrangement loopLengthFrames sound)
      halv = halve (singleSoundArrangement loopLengthFrames sound)
      soundArr = singleSoundArrangement loopLengthFrames sound
      sound2 = (someSounds !! 1)
      soundArr2 = singleSoundArrangement loopLengthFrames sound2
  doubS <- renderArrangement doub
  -- let quad = double (singleSoundArrangement loopLengthFrames doubS)
  --     arr = seqArrangement [snd, doub, halv, parArrangement [snd, doub], parArrangement [snd, doub, halv]]
  --let arr = rev (eqDice soundArr 8)
  arr <- chopOut (eqDice soundArr 16) 0.5
  --msp soundArr
  --msp arr
  msp "HEYO"
  shv soundArr
  shv soundArr2
  shv arr

  --let arr' = seqArrangement [soundArr, clickTrackArr, arr]
  --let arr' = seqArrangement [clickTrackArr, soundArr, soundArr, arr, arr, parArrangement [soundArr, soundArr, arr]]
  let arr' = seqArrangement [clickTrackArr, soundArr, (parArrangement [soundArr, soundArr2]), (parArrangement [soundArr, soundArr2]),
                                            arr, (parArrangement [arr, soundArr2]), (parArrangement [arr, soundArr2])]
  -- let arr' = arr

  --let arr = seqArrangement (map (singleSoundArrangement loopLengthFrames) [sound, sound'])
  -- let acc = acceptable s
  --     accSounds = map (map ((sounds s) !!)) acc
  --     arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleSoundArrangement loopLengthFrames) ss)) accSounds
  --songMix <- renderArrangement $ parArrangement [arr', clickTrackArr]
  songMix <- renderArrangement arr'
  setSound (looper s) songMix
  where dub x = seqArrangement [x, x]
        --addClickMaybe arr = parArrangement [arr, clickTrackArr]
        -- addClickMaybe arr = case addClick of (Just s) -> parArrangement [arr, (singleSoundArrangement loopLengthFrames s)]
        --                                      Nothing -> arr
  
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
like s = nextFromStack $ s { likes = S.insert (currentGroup s) (likes s) }
dislike :: State -> State
dislike s | length (currentGroup s) < 3 = nextFromStack $ s { dislikes = S.insert (currentGroup s) (dislikes s) }
dislike s | otherwise = nextFromStack $ pushStackN s (allSubs (currentGroup s))
  where -- A sub is the list with one element removed
        allSubs :: [Int] -> [[Int]]
        allSubs (x : xs) = [xs] ++ map (x:) (allSubs xs)
        allSubs [] = []

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
        affS = "Affinities:\n" ++ intercalate "\n" (map show (bigToSmall $ acceptable s))
        logS = bar ++ "\n" ++ (intercalate "\n" (extend (reverse $ editorLog s))) ++ "\n" ++ bar
          where extend :: [String] -> [String]
                extend lines = take editorLogLength $ lines ++ (repeat "")
        showList xs = intercalate " " (map show xs)
        bar = "======================"
        bigToSmall :: [[a]] -> [[a]]
        bigToSmall = reverse . sortOn length

revAsc :: String -> String
revAsc s = "\ESC[7m" ++ s ++ "\ESC[0m" 

-- box :: [Int] -> Int -> String
-- box group i = rv i ("[" ++ (fmt i) ++ "]")
--   where fmt i = printf "%4d" i
--         rv :: Int -> String -> String
--         rv i s = if elem i group then rev s else s
box :: Bool -> Int -> String
box reverse i =
  let base = "[" ++ (fmt i) ++ "]"
   in if reverse then revAsc base else base
  where fmt i = printf "%4d" i

boxShowMember :: [Int] -> Int -> String
boxShowMember group i = box (elem i group) i

grid :: State -> String
grid (State { sounds, currentGroup }) = intercalate "\n" $ map format $ splitUp 10 $ map (boxShowMember currentGroup) [0..length sounds - 1]
  where format xs = intercalate " " xs
        splitUp :: Int -> [a] -> [[a]]
        splitUp n [] = []
        splitUp n xs = take n xs : splitUp n (drop n xs)

initGfx :: Int -> Gfx
initGfx n = Gfx (zipWith init fs fs')
  where init f f' = Node { pos = toPt f, dest = toPt f' }
        fs = map (/ fromIntegral n) (map fromIntegral [0..n-1])
        fs' = take n (drop (n `div` 2) (cycle fs))
        toPt f = (V2 80.0 80.0) + (220 *^ V2 (cos a) (sin a))
          where a = f * 2 * pi

affinityMain :: Int -> IO ()
affinityMain seed = do
  withLooper $ \looper -> withGui (initGfx 2000) $ \gfxChan -> do
                    threadDelay $ 20 * 1000000
                    --s <- initState looper
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
