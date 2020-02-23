{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Affinity
( affinityMain
, State(..) ) where

import Control.Concurrent
import Data.List (intercalate, transpose, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Graphics.Gloss
import Linear
import System.Directory (listDirectory)

import Ascii
import Arrangement
import Constants
import Dice
import FX
import Gui
--import Graph
import Looper
import Score
import Sound
import State
import TUI
import Util
import Viz

addClick :: Maybe String
addClick = Nothing
--addClick = Just "looper/1-7.wav"

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
  --msp "YOSH"
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
  resetTerm
  putStrLn $ displayer s'
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

-- This one only picks from the set of loops that aren't part of a like
-- randomGroup s = do
--   let inUse = nodes (likes s)
--       unused = S.toList $ (S.fromList [0..length (sounds s)-1]) `S.difference` inUse
--   groupSize <- getStdRandom (randomR (2,4)) :: IO Int
--   indices <- mapM (\_ -> randFromList unused) [0..groupSize-1]
--   return indices

displayer :: Displayer State
displayer s = intercalate "\n" lines
  where lines = [gridS, bar, currentS, likesS, dislikesS, stackS, bar, affS, logS]
        gridS = grid s
        currentS = "Current: " ++ showList (currentGroup s)
        likesS = "Likes: " ++ showList (map showList (S.toList (likes s)))
        dislikesS = "Dislikes: " ++ showList (map showList (S.toList (dislikes s)))
        stackS = "Stack: " ++ showList (map showList (stack s))
        affS = "Affinities:\n" ++ intercalate "\n" (map show (bigToSmall $ acceptable s))
        logS = bar ++ "\n" ++ (intercalate "\n" (renderEdLog s)) ++ "\n" ++ bar
        showList xs = intercalate " " (map show xs)
        bar = "======================"
        bigToSmall :: [[a]] -> [[a]]
        bigToSmall = reverse . sortOn length
        grid :: State -> String
        grid (State { sounds, currentGroup }) = gridder (length sounds) (flip elem currentGroup)


-- type KeyboardHandler s = s -> Char -> IO (KHResult s)
-- wrappedKeyboardHandler :: (State -> Char -> IO (KHResult State)) -> (State -> Char -> IO (KHResult State))
-- wrappedKeyboardHandler kh s c = do
--   result <- kh s c
--   case result of SetState s' -> do respondToStateChange

affinityMain :: Int -> IO ()
affinityMain seed = do
  withLooper $ \looper -> do
                    s <- initState looper
                    vizMain s (keyboardHandlerWrapper keyboardHandler)
                    --gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
