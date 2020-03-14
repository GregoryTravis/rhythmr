{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Affinity
( affinityMain
, State(..) ) where

import Control.Concurrent
import Control.Monad (replicateM)
import Data.List (intercalate, transpose, sortOn, elemIndex, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Graphics.Gloss
import Linear
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory (listDirectory)

import Ascii
import Arrangement
import Constants
import Dice
import FX
import Gui
import Loop
import Looper
import Memoize (memoizeIO)
import SaveLoad
import Score
import Sound
import State
import Util
import Viz

poolSize = 16 -- 128

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

makeLoader :: (String -> IO Sound) -> Looper -> Loader State StateRep
makeLoader soundLoader looper (StateRep { repLoops, repLikes, repDislikes }) = do
  return $ State { soundLoader, looper, loops = repLoops, likes = repLikes, dislikes = repDislikes, currentGroup = [], stack = [], editorLog = ["Welcome to autobeat"] }

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
  loops <- loadLoops
  return $ State { soundLoader, looper, loops, likes = S.empty, dislikes = S.empty,
                   currentGroup = [], editorLog = ["Welcome to autobeat"], stack = [] }

-- setState s = return (Just s, DoNothing)
-- retCommand c = return (Nothing, c)
setState s = return $ NewState s
retCommand c = return c

-- TODO maybe function type aliases are not good
keyboardHandler :: (State -> Char -> IO (GuiCommand State))
--keyboardHandler :: KeyboardHandler State
keyboardHandler s 'r' = do
  group <- randomGroup s
  let s' = s { currentGroup = group }
  --msp "YOSH"
  --playCurrent s'
  setState s'
keyboardHandler s 'E' = do s' <- newPool s
                           setState s'
keyboardHandler s 'y' = setState $ like s
keyboardHandler s 'n' = setState $ dislike s
keyboardHandler s 'A' = do
  case acceptable s of [] -> setState s
                       (g:gs) -> do
                                   let s' = s { currentGroup = g }
                                   setState s'
keyboardHandler s 'S' = do
  playSong s
  setState s
keyboardHandler s '\ESC' = retCommand Quit
keyboardHandler s 'p' = do
  let s' = nextFromStack $ pushCurrentGroup s
  --msp ("eh", currentGroup s, stack s)
  --msp ("eh", currentGroup s', stack s')
  setState s'
keyboardHandler s ' ' = do
  s' <- if stack s == []
             then return $ edlog s "Stack empty, yo"
             else return $ nextFromStack s
  --let s' = nextFromStack s
  setState s'
keyboardHandler s 'u' = retCommand Undo
keyboardHandler s '\DC2' = retCommand Redo
keyboardHandler s 's' = retCommand $ Save "history.ab"
keyboardHandler s 'L' = retCommand $ Load "history.ab"
keyboardHandler s 'C' = let s' = (combineAffinities s) in setState s'
keyboardHandler s key = setState s'
  where s' = edlog s ("?? " ++ (show key))

-- Replace the pool with a new random selection -- except keep the ones that
-- have already been liked/disliked
newPool :: State -> IO State
newPool s@(State { likes, dislikes }) = do
  let loopsToKeep :: [Loop]
      loopsToKeep = nub $ concat (S.toList likes ++ S.toList dislikes)
  newLoops <- loadRandomLoops (poolSize - length loopsToKeep)
  return $ s { loops = loopsToKeep ++ newLoops, currentGroup = [], stack = [] }

respondToStateChange :: State -> State -> IO ()
respondToStateChange s s' = do
  --resetTerm
  putStrLn $ displayer s'
  if currentGroup s' /= currentGroup s && currentGroup s' /= []
      then playCurrent s'
      else return ()

playSong :: State -> IO ()
playSong s = do
  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readSound filename
                                 Nothing -> return []
  let clickTrackArr = parArrangement (map (singleSoundArrangement loopLengthFrames) clickTrack)
  -- let sis = currentGroup s -- should be affinity group or something / 68
  --     someSounds = map ((sounds s) !!) sis
  someSounds <- loadLoopSounds (soundLoader s) (currentGroup s)
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
  where lines = [gridS, bar, currentS, likesS, dislikesS, stackS, bar, affS, logS, loopsS]
        gridS = grid s
        currentS = "Current: " ++ showLoops (currentGroup s)
        likesS = "Likes: " ++ showList (map showLoops (S.toList (likes s)))
        dislikesS = "Dislikes: " ++ showList (map showLoops (S.toList (dislikes s)))
        stackS = "Stack: " ++ showList (map showLoops (stack s))
        affS = "Affinities:\n" ++ intercalate "\n" (map show ((map . map) inxOf (bigToSmall $ acceptable s)))
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
                    guiMain s saver loader statesToViz' renderViz' updateViz keyboardHandler respondToStateChange 
                    --gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
