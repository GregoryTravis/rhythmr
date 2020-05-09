{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Affinity
( affinityMain
, State(..) ) where

import Control.Concurrent
import Control.Monad (replicateM)
import Data.IORef
import Data.List (intercalate, transpose, sortOn, elemIndex, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import Graphics.Gloss
import Linear
import Linear.Matrix (identity)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory (listDirectory)
import System.Random

import Animate
import Ascii
import Arrangement
import Constants
import Dice
import FX
import Gui
import Hypercube
import Loop
import Looper
import Memoize (memoizeIO)
import SaveLoad
import Score
import Sound
import State
import Util
import Viz

poolSize = 64

addClick :: Maybe String
addClick = Nothing
--addClick = Just "looper/1-7.wav"

-- Suitable for persisting
data StateRep = 
  StateRep { repLoops :: [Loop]
           , repLikes :: S.Set [Loop]
           , repDislikes :: S.Set [Loop] }
  deriving (Read, Show)

emptyStateRep = StateRep { repLoops = [], repLikes = S.empty, repDislikes = S.empty }

initRand :: StdGen
initRand = mkStdGen 0

makeLoader :: (String -> IO Sound) -> Looper -> Loader State StateRep
makeLoader soundLoader looper (StateRep { repLoops, repLikes, repDislikes }) = do
  let mat = identity :: Mat
  matRef <- newIORef mat
  return $ State { soundLoader, looper, loops = repLoops, likes = repLikes, dislikes = repDislikes, currentGroup = [],
                   stack = [], editorLog = ["Welcome to autobeat"], currentSong = Nothing, affinityCycle = 0,
                   currentHypercubeMat = matRef, rand = initRand, strategy = Nothing }

-- saver :: [State] -> [StateRep]
-- saver = map toRep

saver :: State -> StateRep
saver (State { loops, likes, dislikes }) = (StateRep { repLoops = loops, repLikes = likes, repDislikes = dislikes })

-- loadLoops :: (String -> IO Sound) -> IO [Sound]
-- loadLoops soundReader = do
--   filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
--   mapM soundReader filenames

loadLoopSounds ::(String -> IO Sound) -> [Loop] -> IO [Sound] 
loadLoopSounds soundLoader loops = mapM soundLoader (map fn loops)
  where fn (Loop filename) = filename

loadRandomLoops :: Int -> IO [Loop]
loadRandomLoops n = do
  allFilenames <- fmap (map ("loops/" ++)) $ listDirectory "loops"
  filenames <- replicateM n (randFromList allFilenames)
  return $ map Loop filenames

loadLoops :: IO [Loop]
loadLoops = do
  filenames <- fmap (map ("loops/" ++)) $ fmap (take poolSize) $ listDirectory "loops"
  return $ map Loop filenames

initState :: (String -> IO Sound) -> Looper -> IO State
initState soundLoader looper = do
  let mat = identity :: Mat
  matRef <- newIORef mat
  newPool $ State { soundLoader, looper, loops = [], likes = S.empty, dislikes = S.empty,
                    currentGroup = [], editorLog = ["Welcome to autobeat"], stack = [],
                    currentSong = Nothing, affinityCycle = 0, currentHypercubeMat = matRef, rand = initRand, strategy = Nothing }

-- setState s = return (Just s, DoNothing)
-- retCommand c = return (Nothing, c)
setState s = return $ NewState s
retCommand c = return c

-- TODO maybe function type aliases are not good
keyboardHandler :: State -> Char -> IO (GuiCommand State)
--keyboardHandler :: KeyboardHandler State
--keyboardHandler s 'r' = do
--  group <- randomGroup s
--  let s' = s { currentGroup = group, currentSong = Nothing }
--  --msp "YOSH"
--  --playCurrent s'
--  setState s'
keyboardHandler s 'E' = do s' <- newPool s
                           setState s'
keyboardHandler s ' ' = setState $ skip s (Just RandomStrategy)
keyboardHandler s '\ETX' = setState $ like s Nothing
keyboardHandler s 'r' = setState $ like s (Just RandomStrategy)
keyboardHandler s 'i' = setState $ like s (Just IncrementalStrategy)
keyboardHandler s '\STX' = setState $ dislike s Nothing
keyboardHandler s 's' = setState $ dislike s (Just SubsetsStrategy)
keyboardHandler s 'd' = setState $ dislike s (Just DNCStrategy)
-- Hear the top affinity group
keyboardHandler s 'A' = do
  case affinities s of [] -> setState s
                       (g:gs) -> do
                                   let s' = s { currentGroup = g }
                                   setState s'
keyboardHandler s 'W' = do
  writeCurrentSong s
  setState s
keyboardHandler s 'S' = do
  setState (setSong s)
keyboardHandler s '\ESC' = retCommand Quit
--keyboardHandler s 'p' = do
--  let s' = nextFromStack $ pushCurrentGroup s
--  --msp ("eh", currentGroup s, stack s)
--  --msp ("eh", currentGroup s', stack s')
--  setState s'
--keyboardHandler s ' ' = do
--  s' <- if stack s == []
--             then return $ edlog s "Stack empty, yo"
--             else return $ nextFromStack s
--  --let s' = nextFromStack s
--  setState s'
keyboardHandler s 'u' = retCommand Undo
keyboardHandler s '\DC2' = retCommand Redo
keyboardHandler s '\DC3' = retCommand $ Save "history.ab"
keyboardHandler s 'L' = retCommand $ Load "history.ab"
--keyboardHandler s 'C' = let s' = (combineAffinities s) in setState s'
keyboardHandler s 'c' = setState $ setSong $ s { affinityCycle = affinityCycle s + 1 }
keyboardHandler s key = do
  msp $ ("?? " ++ (show key))
  setState s'
  where s' = edlog s ("?? " ++ (show key))

-- Replace the pool with a new random selection -- except keep the ones that
-- have already been liked/disliked
newPool :: State -> IO State
newPool s@(State { likes, dislikes }) = do
  let loopsToKeep :: [Loop]
      loopsToKeep = concat (S.toList likes ++ S.toList dislikes)
  newLoops <- completeList loopsToKeep loadRandomLoops poolSize
  return $ s { loops = nub (loopsToKeep ++ newLoops), currentGroup = [], stack = [] }

-- Complete a list by adding enough elements to reach the given total. Since
-- removing duplicates might cause the total to be too low, we keep requesting
-- until we have enough.
completeList :: Eq a => [a] -> (Int -> IO [a]) -> Int -> IO [a]
completeList soFar getElements total | length soFar == total = return soFar
completeList soFar getElements total | otherwise = do
  newElems <- getElements (total - length soFar)
  let newSoFar = nub $ soFar ++ newElems
  --massert "looping in completeList" (length newSoFar > length soFar)
  completeList newSoFar getElements total

respondToStateChange :: State -> State -> IO ()
respondToStateChange s s' = do
  --resetTerm
  putStrLn $ displayer s'
  if currentSong s' /= currentSong s && currentSong s' /= Nothing
     then playCurrentSong s'
     else if currentGroup s' /= currentGroup s && currentGroup s' /= []
            then playCurrent s'
            else return ()

playCurrentSong :: State -> IO ()
playCurrentSong s@(State { currentSong = Just (score, loops) }) = do
  sounds <- mapM (loadLoopSounds (soundLoader s)) loops
  arr <- renderScore score sounds
  mix <- renderArrangement arr
  setSound (looper s) mix
playCurrentSong s@(State { currentSong = Nothing }) = return ()

writeCurrentSong :: State -> IO ()
writeCurrentSong s = do
  mix <- getSound (looper s)
  case mix of Just mix -> do
                MkSystemTime { systemSeconds } <- getSystemTime
                let filename = "song-" ++ show systemSeconds ++ ".wav"
                writeSound filename mix
              Nothing -> return ()

setSong :: State -> State
setSong s =
  let score = Score [[Measure (0, 0) (Reverb 85)],
                     [Measure (0, 0) (Reverb 85), Measure (0, 1) (Tremolo 10 40), Measure (0, 2) MCompand],
                     [Measure (0, 0) (Reverb 85)],
                     [Measure (0, 0) (Reverb 85), Measure (0, 1) (Tremolo 10 40), Measure (0, 2) MCompand],
                     [Measure (0, 1) (Tremolo 10 40), Measure (0, 2) MCompand, Measure (0, 3) revReverb],
                     [Measure (0, 2) MCompand, Measure (0, 3) revReverb],
                     [Measure (1, 0) (Reverb 85)],
                     [Measure (1, 0) (Reverb 85), Measure (1, 1) (Tremolo 10 40)],
                     [Measure (1, 0) (Reverb 85)],
                     [Measure (1, 0) (Reverb 85), Measure (1, 1) (Tremolo 10 40)],
                     [Measure (1, 2) MCompand, Measure (1, 3) revReverb],
                     [Measure (1, 3) revReverb],
                     [Measure (1, 0) (Reverb 85), Measure (1, 3) revReverb],
                     [Measure (1, 1) (Reverb 85), Measure (1, 3) revReverb],
                     [Measure (0, 0) (Reverb 85), Measure (0, 1) (Tremolo 10 40), Measure (0, 2) MCompand],
                     [Measure (0, 0) (Reverb 85)],
                     [Measure (0, 0) (Reverb 85), Measure (0, 1) (Tremolo 10 40), Measure (0, 2) MCompand],
                     [Measure (0, 1) (Tremolo 10 40), Measure (0, 2) MCompand, Measure (0, 3) revReverb],
                     [Measure (0, 2) MCompand, Measure (0, 3) revReverb],
                     [Measure (1, 0) (Reverb 85)],
                     [Measure (1, 0) (Reverb 85), Measure (1, 1) (Tremolo 10 40)],
                     [Measure (1, 0) (Reverb 85)],
                     [Measure (1, 0) (Reverb 85), Measure (1, 1) (Tremolo 10 40)],
                     [Measure (1, 2) MCompand, Measure (1, 3) revReverb],
                     [Measure (1, 3) revReverb],
                     [Measure (1, 0) (Reverb 85), Measure (1, 3) revReverb],
                     [Measure (1, 1) (Reverb 85), Measure (1, 3) revReverb],
                     [Measure (1, 2) (Reverb 85), Measure (1, 3) revReverb],
                     [Measure (1, 0) (Reverb 85), Measure (1, 2) (Reverb 85), Measure (1, 3) revReverb]]
      revReverb = FXs [Reverse, Reverb 85, Reverse]
   in s { currentSong = Just (score, someAcceptable s) }

-- Of all acceptable groups, pick the last one that has at least 4 elements
someAcceptable :: State -> [[Loop]]
someAcceptable s = take 2 $ reverse $ rotateMod ac $ filter atLeastFour $ affinities s
  where atLeastFour l = length l >= 4
        ac = affinityCycle s

--playSong :: State -> IO ()
--playSong s = do
--  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readSound filename
--                                 Nothing -> return []
--  let clickTrackArr = parArrangement (map (singleSoundArrangement loopLengthFrames) clickTrack)
--  -- let sis = currentGroup s -- should be affinity group or something / 68
--  --     someSounds = map ((sounds s) !!) sis
--  someSounds <- loadLoopSounds (soundLoader s) (currentGroup s)
--  let score = Score [[Measure 0 NoFX],
--                     [Measure 0 (Reverb 85)],
--                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40)],
--                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40), Measure 2 MCompand],
--                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40), Measure 2 MCompand, Measure 3 revReverb]]
--      revReverb = FXs [Reverse, Reverb 85, Reverse]
--  --arr <- renderScore score someSounds
--  let sound = (someSounds !! 0)
--      snd = singleSoundArrangement loopLengthFrames sound
--      doub = double (singleSoundArrangement loopLengthFrames sound)
--      halv = halve (singleSoundArrangement loopLengthFrames sound)
--      soundArr = singleSoundArrangement loopLengthFrames sound
--      sound2 = (someSounds !! 1)
--      soundArr2 = singleSoundArrangement loopLengthFrames sound2
--  doubS <- renderArrangement doub
--  -- let quad = double (singleSoundArrangement loopLengthFrames doubS)
--  --     arr = seqArrangement [snd, doub, halv, parArrangement [snd, doub], parArrangement [snd, doub, halv]]
--  --let arr = rev (eqDice soundArr 8)
--  arr <- chopOut (eqDice soundArr 16) 0.5
--  --msp soundArr
--  --msp arr
--  msp "HEYO"

--  --let arr' = seqArrangement [soundArr, clickTrackArr, arr]
--  --let arr' = seqArrangement [clickTrackArr, soundArr, soundArr, arr, arr, parArrangement [soundArr, soundArr, arr]]
--  let arr' = seqArrangement [clickTrackArr, soundArr, (parArrangement [soundArr, soundArr2]), (parArrangement [soundArr, soundArr2]),
--                                            arr, (parArrangement [arr, soundArr2]), (parArrangement [arr, soundArr2])]
--  -- let arr' = arr

--  --let arr = seqArrangement (map (singleSoundArrangement loopLengthFrames) [sound, sound'])
--  -- let acc = acceptable s
--  --     accSounds = map (map ((sounds s) !!)) acc
--  --     arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleSoundArrangement loopLengthFrames) ss)) accSounds
--  --songMix <- renderArrangement $ parArrangement [arr', clickTrackArr]
--  songMix <- renderArrangement arr'
--  setSound (looper s) songMix
--  where dub x = seqArrangement [x, x]
--        --addClickMaybe arr = parArrangement [arr, clickTrackArr]
--        -- addClickMaybe arr = case addClick of (Just s) -> parArrangement [arr, (singleSoundArrangement loopLengthFrames s)]
--        --                                      Nothing -> arr
  
playCurrent :: State -> IO ()
playCurrent s = do
  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readSound filename
                                 Nothing -> return []
  ss <- loadLoopSounds (soundLoader s) (currentGroup s)
  let -- ss :: [Sound]
      -- ss = map ((sounds s) !!) (currentGroup s)
      arr :: Arrangement
      arr = parArrangement (map (singleSoundArrangement loopLengthFrames) (clickTrack ++ ss))
  mix <- renderArrangement arr
  --msp "setting sound"
  setSound (looper s) mix

-- This one only picks from the set of loops that aren't part of a like
-- randomGroup s = do
--   let inUse = nodes (likes s)
--       unused = S.toList $ (S.fromList [0..length (sounds s)-1]) `S.difference` inUse
--   groupSize <- getStdRandom (randomR (2,4)) :: IO Int
--   indices <- mapM (\_ -> randFromList unused) [0..groupSize-1]
--   return indices

setCursorPos x y = setCursorPosition y x
resetTerm = do
  setCursorPos 0 0
  clearScreen

displayer :: State -> String
displayer s = intercalate "\n" lines
  where lines = [gridS, bar, currentS, likesS, dislikesS, stackS, bar, affS, logS]
        gridS = grid s
        currentS = "Current: " ++ showLoops (currentGroup s)
        likesS = "Likes: " ++ showList (map showLoops (S.toList (likes s)))
        dislikesS = "Dislikes: " ++ showList (map showLoops (S.toList (dislikes s)))
        stackS = "Stack: " ++ showList (map showLoops (stack s))
        affS = "Affinities:\n" ++ intercalate "\n" (map show ((map . map) inxOf (bigToSmall $ affinities s)))
        logS = bar ++ "\n" ++ (intercalate "\n" (renderEdLog s)) ++ "\n" ++ bar
        loopsS = intercalate "\n" (map loopFilename (loops s))
        showList xs = intercalate " " (map show xs)
        bar = "======================"
        bigToSmall :: [[a]] -> [[a]]
        bigToSmall = reverse . sortOn length
        grid :: State -> String
        --grid (State { loops, currentGroup }) = gridder (length loops) (flip elem currentGroup . fromJust . flip elemIndex (loops s))
        grid (State { loops, currentGroup }) = gridder (length loops) (\i -> elem (loops !! i) currentGroup)
        inxOf :: Loop -> Int
        inxOf loop = fromJust $ elemIndex loop (loops s)
        showLoops :: [Loop] -> String
        showLoops loops = showList $ map inxOf loops

-- type KeyboardHandler s = s -> Char -> IO (KHResult s)
-- wrappedKeyboardHandler :: (State -> Char -> IO (KHResult State)) -> (State -> Char -> IO (KHResult State))
-- wrappedKeyboardHandler kh s c = do
--   result <- kh s c
--   case result of SetState s' -> do respondToStateChange

affinityMain :: Int -> IO ()
affinityMain seed = do
  withLooper $ \looper -> do
                    soundLoader <- memoizeIO readSound
                    let loader = makeLoader soundLoader looper
                    s <- initState soundLoader looper
                    guiMain s initViz saver loader stateToViz renderViz keyboardHandler respondToStateChange 
                    --gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
