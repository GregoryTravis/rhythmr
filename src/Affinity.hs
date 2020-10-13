{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Affinity
( affinityMain
, State(..) ) where

import Control.Concurrent
import Control.Monad (replicateM, zipWithM_)
import Data.Binary
import Data.Containers.ListUtils (nubOrd)
import Data.IORef
import Data.List (intercalate, intersect, transpose, sortOn, elemIndex, nub, inits, isInfixOf, splitAt)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.StorableVector as SV
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Graphics.Gloss.Interface.IO.Game (SpecialKey(..), Key(..), Modifiers(..), KeyState(..))
import Linear
import Linear.Matrix (identity)
import System.Environment (getEnv)
import System.Directory (listDirectory, getCurrentDirectory)
import System.Random

import Animate
import Ascii
import Chew
import Constants
import FX
import Gui
import Graph
import Hypercube
import Loop
import Looper
import Memoize (memoizeIO, emptyMemoDir)
import Project
import Rc
import SaveLoad
import State
import Util
import Viz
import Zounds
import qualified Zounds as Z

poolSize = 256

addClick :: Maybe String
addClick = Nothing
--addClick = Just "looper/1-7.wav"

-- Suitable for persisting
data StateRep = 
  StateRep { repCollections :: [(Double, String)]
           , repLoops :: [Loop]
           , repLikes :: [[Loop]]
           , repDislikes :: S.Set [Loop]
           , repCurrentGroup :: [Loop] }
  deriving (Read, Show, Generic)

instance Binary StateRep

emptyStateRep = StateRep { repLoops = [], repLikes = [], repDislikes = S.empty, repCollections = [], repCurrentGroup = [] }

initRand :: StdGen
initRand = mkStdGen 0

makeLoader :: String -> (String -> IO Zound) -> Looper -> Loader State StateRep
makeLoader projectDir soundLoader looper (StateRep { repLoops, repLikes, repDislikes, repCollections, repCurrentGroup }) = do
  let mat = identity :: Mat
  matRef <- newIORef mat
  --msp ("YEP", projectDir)
  return $ State { projectDir, soundLoader, looper, loops = repLoops, likes = repLikes, dislikes = repDislikes, currentGroup = repCurrentGroup,
                   stack = [], editorLog = ["Welcome to Rhythmr"], currentSong = Nothing, affinityCycle = 0,
                   currentHypercubeMat = matRef, rand = initRand, strategy = Nothing, collections = repCollections, useFiz = False }

-- saver :: [State] -> [StateRep]
-- saver = map toRep

saver :: State -> StateRep
saver (State { loops, likes, dislikes, collections, currentGroup }) = (StateRep { repLoops = loops, repLikes = likes, repDislikes = dislikes, repCollections = collections, repCurrentGroup = currentGroup })

-- loadLoops :: (String -> IO Zound) -> IO [Zound]
-- loadLoops soundReader = do
--   filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
--   mapM soundReader filenames

-- (a -> m b) -> t a -> m (t b)
-- (a -> IO b) -> [a] -> IO [b]
-- Given a weighted list of collections, return the collections' contents, with the same weights
scanCollections :: State -> IO [(Double, [String])]
scanCollections s = mapM scan (collections s)
  where scan :: (Double, String) -> IO (Double, [String])
        scan (w, collection) = do
          loopDir <- getLoopDir (projectDir s) collection
          basenames <- listDirectory loopDir
          --msp ("loopDir", loopDir)
          --msp ("basenames", basenames)
          let paths :: [FilePath]
              paths = map ((collection ++ "/") ++) basenames
          --msp ("paths", paths)
          return (w, paths)

loadRandomLoops :: State -> Int -> IO [Loop]
loadRandomLoops s n = do
  weightedFileLists <- scanCollections s
  filenames <- replicateM n (weightedRandFromLists weightedFileLists)
  return $ map Loop filenames

initState :: String -> (String -> IO Zound) -> Looper -> [(Double, String)] -> IO State
initState projectDir soundLoader looper collections = do
  --msp ("YEP2", projectDir)
  let mat = identity :: Mat
  matRef <- newIORef mat
  newPool $ State { projectDir, soundLoader, looper, loops = [], likes = [], dislikes = S.empty,
                    currentGroup = [], editorLog = ["Welcome to Rhythmr"], stack = [],
                    collections,
                    currentSong = Nothing, affinityCycle = 0, currentHypercubeMat = matRef, rand = initRand, strategy = Nothing, useFiz = False }

-- setState s = return (Just s, DoNothing)
-- retCommand c = return (Nothing, c)
setState :: State -> IO (GuiCommand State)
setState s = return $ NewState s
retCommand c = return c

noM :: Modifiers -> Bool
noM m = m == Modifiers { shift = Up, ctrl = Up, alt = Up }
shiftM :: Modifiers -> Bool
shiftM m = m == Modifiers { shift = Down, ctrl = Up, alt = Up }
ctrlM :: Modifiers -> Bool
ctrlM m = m == Modifiers { shift = Up, ctrl = Down, alt = Up }
shiftCtrlM :: Modifiers -> Bool
shiftCtrlM m = m == Modifiers { shift = Down, ctrl = Down, alt = Up }

demoModeKeys :: [(Key, Modifiers -> Bool)]
demoModeKeys =
  [ (Char 'u', noM)
  , (Char '\NAK', shiftCtrlM)
  , (Char '\DC2', ctrlM)
  , (Char '\DC2', shiftCtrlM)
  , (SpecialKey KeyEsc, noM)
  , (Char 'S', shiftM)
  , (Char 'c', noM)
  , (Char 'A', noM)
  , (Char 'a', noM)
  , (Char '0', noM)
  , (Char '1', noM)
  , (Char '2', noM)
  , (Char '3', noM)
  , (Char '4', noM)
  , (Char '5', noM)
  , (Char '6', noM)
  , (Char '7', noM)
  , (Char '8', noM)
  , (Char '9', noM)
  ]

restrictToKeys :: [(Key, Modifiers -> Bool)] ->
                  (State -> (Key, Modifiers) -> IO (GuiCommand State)) ->
                  (State -> (Key, Modifiers) -> IO (GuiCommand State))
restrictToKeys allowed h s input | any (match input) allowed = h s input
                                 | otherwise = return DoNothing
  where match :: (Key, Modifiers) -> (Key, Modifiers -> Bool) -> Bool
        match (k, m) (k', mp) = k == k' && mp m

demoKeyboardHandler :: State -> (Key, Modifiers) -> IO (GuiCommand State)
demoKeyboardHandler = restrictToKeys demoModeKeys keyboardHandler

-- TODO maybe function type aliases are not good
keyboardHandler :: State -> (Key, Modifiers) -> IO (GuiCommand State)
--keyboardHandler :: KeyboardHandler State
--keyboardHandler s 'r' = do
--  group <- randomGroup s
--  let s' = s { currentGroup = group, currentSong = Nothing }
--  --msp "YOSH"
--  --playCurrent s'
--  setState s'
keyboardHandler s (Char 'E', m) | shiftM m = do s' <- newPool s
                                                setState s'
keyboardHandler s (Char 'F', m) | shiftM m = do setState (s { useFiz = not (useFiz s) })
keyboardHandler s (SpecialKey KeySpace, m) | noM m = setState $ skip s (Just RandomStrategy)
keyboardHandler s (SpecialKey KeyRight, m) | noM m = setState $ like s Nothing
keyboardHandler s (Char 'R', m) | shiftM m = setState $ skip s (Just RandomStrategy)
keyboardHandler s (Char 'r', m) | noM m = setState $ skip s (Just Random2Strategy)
keyboardHandler s (Char 'I', m) | shiftM m = setState $ skip s (Just IncrementalStrategy)
keyboardHandler s (Char 'i', m) | noM m = setState $ skip s (Just Incremental2Strategy)
keyboardHandler s (SpecialKey KeyLeft, m) | noM m = setState $ dislike s Nothing
keyboardHandler s (Char 's', m) | noM m = setState $ dislike s (Just SubsetsStrategy)
keyboardHandler s (Char 'd', m) | noM m = setState $ dislike s (Just DNCStrategy)
-- Hear the top affinity group
-- keyboardHandler s (Char 'A', m) | noM m = do
--   case affinities s of [] -> setState s
--                        (g:gs) -> do
--                                    let s' = s { currentGroup = g }
--                                    setState s'
keyboardHandler s (Char 'W', m) | shiftM m = do
  writeCurrentSong s
  -- -- writeCurrentSongSeparateTracks' writes to the desktop, if run from desktop
  -- -- but writeClick can't read clik.wav so it won't work in that case
  -- writeCurrentSongSeparateTracks' s
  -- writeClick
  setState s
keyboardHandler s (Char 'S', m) | shiftM m = cycleLikesSong s >>= setSong s
keyboardHandler s (Char 'T', m) | shiftM m = tallSong s >>= setSong s
keyboardHandler s (Char 'H', m) | shiftM m = thresholdSong s >>= setSong s
keyboardHandler s (Char 'L', m) | shiftM m = likesSong s >>= setSong s
keyboardHandler s (Char 'J', m) | shiftM m = chew s >>= setSong s
keyboardHandler s (Char 'A', m) | shiftM m = hiChew s >>= setSong s
keyboardHandler s (Char 'a', m) | noM m = hiChew s' >>= setSong s'
  where s' = s { affinityCycle = affinityCycle s + 1 }
keyboardHandler s (SpecialKey KeyEsc, m) | noM m = do
  projectFile <- getProjectFile (projectDir s)
  retCommand $ SaveAndQuit projectFile
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
keyboardHandler s (Char 'u', m) | noM m = retCommand Undo
keyboardHandler s (Char '\NAK', m) | shiftCtrlM m = retCommand UndoFully
keyboardHandler s (Char '\DC2', m) | ctrlM m = retCommand Redo
keyboardHandler s (Char '\DC2', m) | shiftCtrlM m = retCommand RedoFully
keyboardHandler s (Char '\DC3', m) | ctrlM m = do
  projectFile <- getProjectFile (projectDir s)
  retCommand $ Save projectFile
keyboardHandler s (Char '\DC1', m) | ctrlM m= retCommand Quit
keyboardHandler s (Char '\DC1', m) | shiftCtrlM m = retCommand QuitWithoutSaving
-- keyboardHandler s 'L' = do
--   file <- getHistoryFile
--   retCommand $ Load file
--keyboardHandler s 'C' = let s' = (combineAffinities s) in setState s'
keyboardHandler s (Char 'c', m) | noM m = cycleLikesSong s' >>= setSong s'
  where s' = s { affinityCycle = affinityCycle s + 1 }
keyboardHandler s (Char c, m) | noM m && c >= '0' && c <= '9' = setVolume' c s
keyboardHandler s (Char c, _) | otherwise = do
  msp $ ("?? " ++ (show c))
  return DoNothing
keyboardHandler _ _ = do
  return DoNothing

setVolume' :: Char -> State -> IO (GuiCommand State)
setVolume' c s = do --msp ("volume", c, volume, fromEnum c - 48)
                    setVolume volume (looper s)
                    return DoNothing
  where volume = fromIntegral (fromEnum c - 48) * (1.0 / 9.0)

-- Replace the pool with a new random selection -- except keep the ones that
-- have already been liked/disliked
newPool :: State -> IO State
newPool s@(State { likes, dislikes }) = do
  let loopsToKeep :: [Loop]
      loopsToKeep = concat likes -- ++ S.toList dislikes)
  newLoops <- loadRandomLoops s (poolSize - length loopsToKeep)
  --msp ("newPool", poolSize, length loopsToKeep, newLoops)
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
  --putStrLn $ displayer s'
  --msp "respondToStateChange"
  if currentGroup s' /= currentGroup s && currentGroup s' /= []
    then playCurrent s'
    else return ()

-- playCurrentSong :: State -> IO ()
-- playCurrentSong (State { currentSong = Nothing }) = return ()
-- playCurrentSong s@(State { currentSong = Just loops }) = do
--   song <- renderLoopGrid s loops
--   mix <- time "zrender" $ strictRender song
--   msp ("mix", durationSeconds mix, desiredLength)
--   time "zsetsound" $ setZound (looper s) mix

-- playCurrentSong :: State -> IO ()
-- playCurrentSong s@(State { currentSong = Just (score, loops) }) = do
--   sounds <- mapM (loadLoopZounds (soundLoader s)) loops
--   arr <- renderScore score sounds
--   mix <- renderArrangement arr
--   setZound (looper s) mix
-- playCurrentSong s@(State { currentSong = Nothing }) = return ()

-- HACK: if the current directory contains "Rhythmr.app" then we're probably
-- running from the icon so write to the desktop. Otherwise, write to the
-- current directory.
getWriteDir :: IO FilePath
getWriteDir = do
  cwd <- getCurrentDirectory
  home <- getEnv "HOME"
  let desktop = home ++ "/Desktop"
  msp ("CWD", cwd)
  msp ("HOME", home)
  if "Rhythmr.app" `isInfixOf` cwd
    then return desktop
    else return cwd

writeCurrentSong :: State -> IO ()
writeCurrentSong s = do
  let mix = snd $ fromJust (currentSong s)
  MkSystemTime { systemSeconds } <- getSystemTime
  dir <- getWriteDir
  let filename = dir ++ "/song-" ++ show systemSeconds ++ ".wav"
  msp ("writing to", filename)
  writeZound filename mix
-- writeCurrentSong s = do
--   let loopGrid = buildLoopGrid s
--   z <- renderLoopGrid s loopGrid
--   mix <- render z
--   MkSystemTime { systemSeconds } <- getSystemTime
--   let filename = "song-" ++ show systemSeconds ++ ".wav"
--   writeZound filename mix

writeCurrentSongSeparateTracks' :: State -> IO ()
writeCurrentSongSeparateTracks' s = do
  dir <- getWriteDir
  stems <- renderStems s
  renderedStems <- mapM render stems
  zipWithM_ (writeIt dir) renderedStems [0..]
    where writeIt dir z i = do
            --msp $ "stem " ++ filename
            msp ("writing to", filename)
            writeZound filename z
            where filename = dir ++ "/stem-" ++ (show i) ++ ".wav"

writeClick :: IO ()
writeClick = do
  dir <- getWriteDir
  clik <- readZound "wavs/clik.wav"
  let z = renderGrid (take desiredLengthLoops (repeat [clik])) bpm
  mix <- render z
  writeZound (dir ++ "/click.wav") mix

--writeCurrentSongSeparateTracks :: State -> IO ()
--writeCurrentSongSeparateTracks s@(State { currentSong = Just (score, loops) }) = do
--  --sounds <- mapM (loadLoopZounds (soundLoader s)) loops
--  mapM_ (writeJustOneZound s loops score) (zip [0..] (concat loops))
--writeCurrentSongSeparateTracks s@(State { currentSong = Nothing }) = do
--  msp "Nothing to write"

-- -- Replace all but one of the sounds with silence, and render and write the stem
-- writeJustOneZound :: State -> [[Loop]] -> Score -> (Int, Loop) -> IO ()
-- writeJustOneZound s loops score (i, loop) = do
--   sound <- case loop of Loop filename -> soundLoader s filename
--   let sounds = map (map replaceDifferent) loops
--       replaceDifferent loop' | loop == loop' = sound
--       replaceDifferent _ | otherwise = silentMeasure
--       filename = "stem-" ++ (show i) ++ ".wav"
--   arr <- renderScore score sounds
--   mix <- renderArrangement arr
--   writeZound filename mix

-- writeCurrentSong :: State -> IO ()
-- writeCurrentSong s = do
--   mix <- getZound (looper s)
--   case mix of Just mix -> do
--                 MkSystemTime { systemSeconds } <- getSystemTime
--                 let filename = "song-" ++ show systemSeconds ++ ".wav"
--                 writeZound filename mix
--               Nothing -> return ()

ramps :: [a] -> [[a]]
ramps = concat . tail . inits . tail . inits

dub :: [a] -> [a]
dub xs = xs ++ xs
-- Used for the FARM peformance
-- dub = id

oneTwoThree [one, two] = dub [[one], [one, two]]
oneTwoThree [one, two, three] = dub [[one], [one, two]] ++ dub [[one, three], [one, two, three]]

cycles :: [a] -> [[a]]
cycles xs = xs : cycles (tail (cycle xs))

allFirstThrees :: [a] -> [[a]]
allFirstThrees xs = take n (map (take 3) (cycles xs))
  where n = length xs

buildLoopGrid :: State -> [[Loop]]
buildLoopGrid s@(State { affinityCycle, likes }) =
  let stacks :: [[Loop]]
      stacks = esp $ nubOrd $ esp $ map (take 3) $ rotateMod affinityCycle likes
      loopGrid :: [[Loop]]
      loopGrid = concat $ (map mini stacks)
   in take desiredLengthLoops loopGrid
  where mini :: (Show a, Ord a) => [a] -> [[a]]
        mini xs@[_, _] = oneTwoThree xs
        mini xs =
          let --cycled :: [a]
              cycled = cycle xs
              --cycles :: [[a]]
              cycles = map (\n -> drop n cycled) [0..length xs - 1]
              --firstThrees :: [[a]]
              firstThrees = map (take 3) cycles
              justOneFirstThree = [head firstThrees]
           in concat $ nubOrd $ map (map nubOrd) $ map oneTwoThree justOneFirstThree

groupBySourceTrack :: [Loop] -> [[Loop]]
--groupBySourceTrack = groupUsing getSourceTrackHash
groupBySourceTrack xs = groupUsing getSourceTrackHash xs

-- Group loops by source track, then render each group separately
buildStemLoopGrids :: State -> [[[Loop]]]
buildStemLoopGrids s =
  let loopGrid = buildLoopGrid s
      loops = nubOrd (concat loopGrid)
      sourceTrackGroups = groupBySourceTrack loops
   in map (onlyThese loopGrid) sourceTrackGroups
  where onlyThese :: [[Loop]] -> [Loop] -> [[Loop]]
        onlyThese loopGrid loops = map (intersect loops) loopGrid

renderStems :: State -> IO [Zound]
renderStems s = do
  let loopGrids = buildStemLoopGrids s
  --msp "HEY"
  --msp loopGrids
  mapM (renderLoopGrid s) loopGrids

renderLoopGrid :: State -> [[Loop]] -> IO Zound
renderLoopGrid s loopGrid = do
  -- let numLoops = length (nubOrd (concat loopGrid))
  -- msp ("loopgrid", numLoops)
  -- let filenameGrid :: [[String]]
  --     filenameGrid = map (map loopFilename) loopGrid
  --     rah :: IO [[Zound]]
  --     rah =  mapM (mapM soundLoader) filenameGrid
  zoundGrid <- ((mapM (mapM (loadLoopZound s)) loopGrid) :: IO [[Zound]])
  let mix :: Zound
      mix = renderGrid zoundGrid bpm
  return mix

---- Build a score that happens to match the current affinityCycle affinities
--buildScore :: State -> Score
--buildScore (State { affinityCycle, likes })  =
--  let stacks = rotateMod affinityCycle (S.toList likes)
--      measures :: [[Measure]]
--      measures = [[Measure (m, p) fx | p <- [0..length (stacks !! m) - 1]] | m <- [0..length stacks - 1]]
--      --ramped = concat $ map ramps measures
--      fx = Reverb 85
--   --in Score $ shew $ concat $ concat $ map oneTwoThree $ shew $ map allFirstThrees (eesp (map length stacks, map length measures) (filter ((>= 3) . length) measures))
--   in Score $ concat $ map mini (filter ((>= 3) . length) measures)
--  where shew xs = eesp (map length xs) xs
--        mini :: [a] -> [[a]]
--        mini xs =
--          let --cycled :: [a]
--              cycled = cycle xs
--              --cycles :: [[a]]
--              cycles = map (\n -> drop n cycled) [0..length xs - 1]
--              --firstThrees :: [[a]]
--              firstThrees = map (take 3) cycles
--              justOneFirstThree = [head firstThrees]
--           in concat $ map oneTwoThree justOneFirstThree

-- number :: [a] -> [(Int, a)]
-- number = zip [0..]

cycleLikesSong :: State -> IO Zound
cycleLikesSong s = do
  renderLoopGrid s (buildLoopGrid s)

--renderLoopGrid :: State -> [[Loop]] -> IO Zound
tallSong :: State -> IO Zound
tallSong s = do
  renderLoopGrid s (buildTallLoopGrid s)

-- Just the likes, in chronological order
likesSong :: State -> IO Zound
likesSong s = do
  renderLoopGrid s (likes s)

-- Maximal paths through connected components
thresholdSong :: State -> IO Zound
thresholdSong s = do
  let walks = thresholdedWalks (likes s)
      best = last (check walks)
      check walks = assertM "thresholdSong" (not (null walks)) walks
  time "msp walks" $ putStrLn $ show walks
  z <- renderLoopGrid s (snd best)
  msp ("k-threshold lengths", map (\(k, walk) -> (k, length walk)) walks)
  return z

buildTallLoopGrid :: State -> [[Loop]]
buildTallLoopGrid s = concat (map movement stacks)
  where stacks = filter ((>= 4) . length) $ likes s
        movement :: [Loop] -> [[Loop]]
        movement stack = [b, b1, b2, b12]
          where (b, a) = halve stack
                (a1, a2) = halve a
                b1 = b ++ a1
                b2 = b ++ a2
                b12 = b ++ a1 ++ a2
                halve xs = splitAt (length xs `div` 2) xs

setSong :: State -> Zound -> IO (GuiCommand State)
setSong s mix = do
  noSound (looper s)
  z <- strictRender mix
  let s' = s { currentSong = Just (mix, z), currentGroup = [] }
  setZoundFromTheTop (looper s') z
  setState s'

-- Of all acceptable groups, pick the last one that has at least 4 elements
someAcceptable :: State -> [[Loop]]
someAcceptable s = take 2 $ reverse $ rotateMod ac $ filter atLeastFour $ affinities s
  where atLeastFour l = length l >= 4
        ac = affinityCycle s

--playSong :: State -> IO ()
--playSong s = do
--  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readZound filename
--                                 Nothing -> return []
--  let clickTrackArr = parArrangement (map (singleZoundArrangement loopLengthFrames) clickTrack)
--  -- let sis = currentGroup s -- should be affinity group or something / 68
--  --     someZounds = map ((sounds s) !!) sis
--  someZounds <- loadLoopZounds (soundLoader s) (currentGroup s)
--  let score = Score [[Measure 0 NoFX],
--                     [Measure 0 (Reverb 85)],
--                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40)],
--                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40), Measure 2 MCompand],
--                     [Measure 0 NoFX, Measure 1 (Tremolo 10 40), Measure 2 MCompand, Measure 3 revReverb]]
--      revReverb = FXs [Reverse, Reverb 85, Reverse]
--  --arr <- renderScore score someZounds
--  let sound = (someZounds !! 0)
--      snd = singleZoundArrangement loopLengthFrames sound
--      doub = double (singleZoundArrangement loopLengthFrames sound)
--      halv = halve (singleZoundArrangement loopLengthFrames sound)
--      soundArr = singleZoundArrangement loopLengthFrames sound
--      sound2 = (someZounds !! 1)
--      soundArr2 = singleZoundArrangement loopLengthFrames sound2
--  doubS <- renderArrangement doub
--  -- let quad = double (singleZoundArrangement loopLengthFrames doubS)
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

--  --let arr = seqArrangement (map (singleZoundArrangement loopLengthFrames) [sound, sound'])
--  -- let acc = acceptable s
--  --     accZounds = map (map ((sounds s) !!)) acc
--  --     arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleZoundArrangement loopLengthFrames) ss)) accZounds
--  --songMix <- renderArrangement $ parArrangement [arr', clickTrackArr]
--  songMix <- renderArrangement arr'
--  setZound (looper s) songMix
--  where dub x = seqArrangement [x, x]
--        --addClickMaybe arr = parArrangement [arr, clickTrackArr]
--        -- addClickMaybe arr = case addClick of (Just s) -> parArrangement [arr, (singleZoundArrangement loopLengthFrames s)]
--        --                                      Nothing -> arr
  
playCurrent :: State -> IO ()
playCurrent s = do
  clickTrack <- case addClick of Just filename -> fmap (:[]) $ readZound filename
                                 Nothing -> return []
  ss <- loadLoopZounds s (currentGroup s)
  let z = renderGrid [ss] bpm
  mix <- render z
  setZound (looper s) mix

-- This one only picks from the set of loops that aren't part of a like
-- randomGroup s = do
--   let inUse = nodes (likes s)
--       unused = S.toList $ (S.fromList [0..length (sounds s)-1]) `S.difference` inUse
--   groupSize <- getStdRandom (randomR (2,4)) :: IO Int
--   indices <- mapM (\_ -> randFromList unused) [0..groupSize-1]
--   return indices

displayer :: State -> String
displayer s = intercalate "\n" lines
  where lines = [gridS, bar, currentS, likesS, dislikesS, stackS, bar, affS, logS]
        gridS = grid s
        currentS = "Current: " ++ showLoops (currentGroup s)
        likesS = "Likes: " ++ showList (map showLoops (likes s))
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

cleanupMemoMaybe :: IO ()
cleanupMemoMaybe = do
  --msp ("CD", cacheDownloads rc)
  if cacheDownloads rc
    then return ()
    else emptyMemoDir
    
-- Scan the project dir for existing collections and merge that with the provided list.
-- Any collection not explicitly assigned a weight is given a default weight of 1.
-- If you 
scanForCollections :: FilePath -> [(Double, String)] -> IO [(Double, String)]
scanForCollections projectDir provided = do
  projectLoopDirs <- getLoopDirs projectDir
  --msp ("WUT", projectLoopDirs)
  let defaultWeights = M.fromList (zip projectLoopDirs (repeat 1))
      providedWeights = M.fromList (map swap provided)
      combinedWeights = providedWeights `M.union` defaultWeights
  return $ map swap (M.toList combinedWeights)

affinityMain :: Bool -> String -> Int -> [(Double, String)] -> IO ()
affinityMain demoMode projectDir seed collections = do
  msp $ "demo mode: " ++ show demoMode
  --msp ("before", collections)
  collections <- scanForCollections projectDir collections
  --msp ("after", collections)
  let kh = if demoMode then demoKeyboardHandler else keyboardHandler
  withLooper $ \looper -> do
                    soundLoader <- memoizeIO readZoundFadeEnds
                    let loader = makeLoader projectDir soundLoader looper
                    s <- initState projectDir soundLoader looper collections
                    projectFile <- getProjectFile projectDir
                    guiMain s (Just projectFile) initViz saver loader stateToViz updateFiz renderViz kh respondToStateChange cleanupMemoMaybe
                    --gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
