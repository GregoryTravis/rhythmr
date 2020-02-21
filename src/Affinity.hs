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
import State
import TUI
import Util

editorLogLength = 10

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

-- This is not used; it is required so that KHResults can be compared
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
  msp "YOSH"
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

gridSizeFor :: Int -> Int
gridSizeFor n = ceiling $ sqrt $ fromIntegral n

unitSquareTo :: V2 Float -> V2 Float -> (Picture -> Picture)
unitSquareTo (V2 llx lly) (V2 w h) picture = Translate llx lly $ Scale w h picture

scaler :: V2 Float -> (V2 Float -> V2 Float)
scaler (V2 x y) (V2 x' y') = V2 (x*x') (y*y')

translater :: V2 Float -> (V2 Float -> V2 Float)
translater (V2 x y) (V2 x' y') = V2 (x+x') (y+y')

gridTransformsForN :: Int -> [V2 Float -> V2 Float]
gridTransformsForN n =
  let gridSize = gridSizeFor n
      ijs = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]
      gbx = V2 gridStep 0
      gby = V2 0 gridStep
      gridStep :: Float
      gridStep = 1.0 / (fromIntegral gridSize)
      translateFor (i, j) = (fromIntegral i *^ gbx) + (fromIntegral j *^ gby)
      scale = V2 gridStep gridStep
      transformFor ij = (translater (translateFor ij)) . (scaler scale)
   in take n (map transformFor ijs)

ringOfCirclesInUnitSquare :: Int -> [V2 Float -> V2 Float]
ringOfCirclesInUnitSquare n = circles
  where circles = map circle [0..n-1]
        circle i = scaler (V2 0.5 0.5) . translater (V2 1 1) . translater offset
          where ang = 2 * pi * (fromIntegral i / fromIntegral n)
                offset = (1.0 - margin - (circleRadius / 2)) *^ V2 (cos ang) (sin ang)
        tr (V2 x y) p = Translate x y p
        circleRadius = 0.15
        margin = 0.3

affinityPositions :: State -> M.Map Int (V2 Float)
-- affinityPositions s = case esp $ acceptable s of xss -> M.fromList (zip (concat xss) (map pos [0..]))
--   where pos i = V2 (fromIntegral i * 5) 0
affinityPositions s = case esp $ acceptable s of xss -> M.fromList $ concat (zipWith rah (gridTransformsForN (length xss)) xss)
  where rah :: (V2 Float -> V2 Float) -> [Int] -> [(Int, V2 Float)]
        rah xform xs = zip xs $ map (\cXform -> ((scaler (V2 400 400)) . xform . cXform) (V2 0 0)) (ringOfCirclesInUnitSquare (length xs))

updateGfx :: GuiState -> GuiState
updateGfx gs = gs { getDings = newDings }
  where newDings = map (\p -> Ding p p) $ map (\k -> M.findWithDefault def k positions) [0..length (sounds (getState gs)) - 1]
        def = V2 0 0
        positions = esp $ affinityPositions (getState gs)

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

affinityMain :: Int -> IO ()
affinityMain seed = do
  withLooper $ \looper -> do
                    s <- initState looper
                    gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
