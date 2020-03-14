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
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory (listDirectory)

import Ascii
import Arrangement
import Constants
import Dice
import FX
import Gui
--import Graph
import Looper
import Memoize (memoizeIO)
import SaveLoad
import Score
import Sound
import State
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

-- -- We do them as a group since we only want to load the sounds once for all of them
-- fromReps :: Looper -> [StateRep] -> IO [State]
-- fromReps looper reps = do
--   sounds <- (loadLoops :: IO [Sound])
--   return $ map (fromRep sounds) reps
--   where fromRep :: [Sound] -> StateRep -> State
--         fromRep sounds (StateRep { repLikes, repDislikes }) =
--           State { sounds, likes = repLikes, dislikes = repDislikes, currentGroup = [], looper,
--                   editorLog = ["Welcome to autobeat"], stack = [] }

----loader :: Loader State [StateRep]
--loader :: State -> [StateRep] -> IO [State]
--loader currentState reps = fromReps (looper currentState) reps
makeLoader :: (String -> IO Sound) -> Looper -> Loader State StateRep
makeLoader soundReader looper (StateRep { repLikes, repDislikes }) = do
  sounds <- loadLoops soundReader
  return $ State { sounds, looper, likes = repLikes, dislikes = repDislikes, currentGroup = [], stack = [], editorLog = ["Welcome to autobeat"] }

-- saver :: [State] -> [StateRep]
-- saver = map toRep

saver :: State -> StateRep
saver (State { likes, dislikes }) = (StateRep { repLikes = likes, repDislikes = dislikes })

loadLoops :: (String -> IO Sound) -> IO [Sound]
loadLoops soundReader = do
  filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
  mapM soundReader filenames

initState :: (String -> IO Sound) -> Looper -> IO State
initState soundReader looper = do
  sounds <- loadLoops soundReader
  return $ State { sounds, looper, likes = S.empty, dislikes = S.empty,
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

respondToStateChange :: State -> State -> IO ()
respondToStateChange s s' = do
  resetTerm
  putStrLn $ displayer s'
  if currentGroup s' /= currentGroup s && currentGroup s' /= []
      then playCurrent s'
      else return ()

--keyboardHandlerWrapper :: (State -> Char -> IO (GuiCommand State)) -> (State -> Char -> IO (GuiCommand State))
----keyboardHandler :: (Char -> State -> IO (State, GuiCommand))
--keyboardHandlerWrapper kh s k = do
--  command <- kh s k
--  case command of NewState s' -> if s /= s'
--                                    then respondToStateChange s s'
--                                    else return ()
--                  _ -> return ()
--  return command

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

setCursorPos x y = setCursorPosition y x
resetTerm = do
  setCursorPos 0 0
  clearScreen

displayer :: State -> String
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
                    soundReader <- memoizeIO readSound
                    let loader = makeLoader soundReader looper
                    s <- initState soundReader looper
                    guiMain s saver loader statesToViz' renderViz' updateViz keyboardHandler respondToStateChange 
                    --gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
