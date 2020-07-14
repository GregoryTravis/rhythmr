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
import Data.List (intercalate, intersect, transpose, sortOn, elemIndex, nub, inits)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.StorableVector as SV
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import GHC.Generics (Generic)
import Graphics.Gloss
import Linear
import Linear.Matrix (identity)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.Random

import Animate
import Ascii
import Chew
import Constants
import FX
import Gui
import Hypercube
import Loop
import Looper
import Memoize (memoizeIO)
import SaveLoad
import Zounds
import State
import Util
import Viz
import qualified Zounds as Z

poolSize = 64

addClick :: Maybe String
addClick = Nothing
--addClick = Just "looper/1-7.wav"

-- Suitable for persisting
data StateRep = 
  StateRep { repCollections :: [(Double, String)]
           , repLoops :: [Loop]
           , repLikes :: S.Set [Loop]
           , repDislikes :: S.Set [Loop]
           , repCurrentGroup :: [Loop] }
  deriving (Read, Show, Generic)

instance Binary StateRep

emptyStateRep = StateRep { repLoops = [], repLikes = S.empty, repDislikes = S.empty, repCollections = [], repCurrentGroup = [] }

initRand :: StdGen
initRand = mkStdGen 0

makeLoader :: String -> (String -> IO Zound) -> Looper -> Loader State StateRep
makeLoader projectFile soundLoader looper (StateRep { repLoops, repLikes, repDislikes, repCollections, repCurrentGroup }) = do
  let mat = identity :: Mat
  matRef <- newIORef mat
  return $ State { projectFile, soundLoader, looper, loops = repLoops, likes = repLikes, dislikes = repDislikes, currentGroup = repCurrentGroup,
                   stack = [], editorLog = ["Welcome to Rhythmr"], currentSong = Nothing, affinityCycle = 0,
                   currentHypercubeMat = matRef, rand = initRand, strategy = Nothing, collections = repCollections }

-- saver :: [State] -> [StateRep]
-- saver = map toRep

saver :: State -> StateRep
saver (State { loops, likes, dislikes, collections, currentGroup }) = (StateRep { repLoops = loops, repLikes = likes, repDislikes = dislikes, repCollections = collections, repCurrentGroup = currentGroup })

-- loadLoops :: (String -> IO Zound) -> IO [Zound]
-- loadLoops soundReader = do
--   filenames <- fmap (map ("loops/" ++)) $ fmap (take 128) $ listDirectory "loops"
--   mapM soundReader filenames

loadLoopZounds ::(String -> IO Zound) -> [Loop] -> IO [Zound] 
loadLoopZounds soundLoader loops = mapM soundLoader (map fn loops)
  where fn (Loop filename) = filename

-- (a -> m b) -> t a -> m (t b)
-- (a -> IO b) -> [a] -> IO [b]
-- Given a weighted list of dirs, return the dirs' contents, with the same weights
scanCollections :: [(Double, String)] -> IO [(Double, [String])]
scanCollections collections = mapM scan collections
  where scan :: (Double, String) -> IO (Double, [String])
        scan (w, dir) = do
          basenames <- listDirectory dir
          let paths :: [FilePath]
              paths = map ((dir ++ "/") ++) basenames
          return (w, paths)

loadRandomLoops :: State -> Int -> IO [Loop]
loadRandomLoops s n = do
  weightedFileLists <- scanCollections (collections s)
  filenames <- replicateM n (weightedRandFromLists weightedFileLists)
  msp ("HAHA", filenames)
  return $ map Loop filenames

-- initState :: String -> (String -> IO Zound) -> Looper -> [(Double, String)] -> IO State
-- initState projectFile soundLoader looper collections = do
--   let mat = identity :: Mat
--   matRef <- newIORef mat
--   newPool $ State { projectFile, soundLoader, looper, loops = [], likes = S.empty, dislikes = S.empty,
--                     currentGroup = [], editorLog = ["Welcome to Rhythmr"], stack = [],
--                     collections,
--                     currentSong = Nothing, affinityCycle = 0, currentHypercubeMat = matRef, rand = initRand, strategy = Nothing }

-- setState s = return (Just s, DoNothing)
-- retCommand c = return (Nothing, c)
setState :: State -> IO (GuiCommand State)
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
keyboardHandler s 'r' = setState $ skip s (Just RandomStrategy)
keyboardHandler s 'i' = setState $ skip s (Just IncrementalStrategy)
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
  writeCurrentSongSeparateTracks' s
  writeClick
  setState s
keyboardHandler s 'S' = cycleLikesSong s >>= setSong s
keyboardHandler s 'J' = chew s >>= setSong s
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
keyboardHandler s '\DC3' = do
  retCommand $ Save (projectFile s)
-- keyboardHandler s 'L' = do
--   file <- getHistoryFile
--   retCommand $ Load file
--keyboardHandler s 'C' = let s' = (combineAffinities s) in setState s'
keyboardHandler s 'c' = (cycleLikesSong $ s { affinityCycle = affinityCycle s + 1 }) >>= setSong s
keyboardHandler s key = do
  msp $ ("?? " ++ (show key))
  setState s'
  where s' = edlog s ("?? " ++ (show key))

-- Replace the pool with a new random selection -- except keep the ones that
-- have already been liked/disliked
newPool :: State -> IO State
newPool s@(State { likes, dislikes }) = do
  let loopsToKeep :: [Loop]
      loopsToKeep = concat (S.toList likes) -- ++ S.toList dislikes)
  msp ("eep", poolSize, length loopsToKeep)
  newLoops <- loadRandomLoops s (poolSize - length loopsToKeep)
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
  msp "respondToStateChange"
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

writeCurrentSong :: State -> IO ()
writeCurrentSong s = do
  let mix = snd $ fromJust (currentSong s)
  MkSystemTime { systemSeconds } <- getSystemTime
  let filename = "song-" ++ show systemSeconds ++ ".wav"
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
  stems <- renderStems s
  renderedStems <- mapM render stems
  zipWithM_ writeIt renderedStems [0..]
    where writeIt z i = do
            msp $ "stem " ++ filename
            writeZound filename z
            where filename = "stem-" ++ (show i) ++ ".wav"

writeClick :: IO ()
writeClick = do
  clik <- readZound "wavs/clik.wav"
  let z = renderGrid (take desiredLengthLoops (repeat [clik])) bpm
  mix <- render z
  writeZound "click.wav" mix

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

oneTwoThree :: [a] -> [[a]]
oneTwoThree xs = dub [[one], [one, two]] ++ dub [[one, three], [one, two, three]]
  where [one, two, three] = xs
        dub xs = xs ++ xs

cycles :: [a] -> [[a]]
cycles xs = xs : cycles (tail (cycle xs))

allFirstThrees :: [a] -> [[a]]
allFirstThrees xs = take n (map (take 3) (cycles xs))
  where n = length xs

buildLoopGrid :: State -> [[Loop]]
buildLoopGrid s@(State { affinityCycle, likes }) =
  let stacks :: [[Loop]]
      stacks = rotateMod affinityCycle (S.toList likes)
      loopGrid :: [[Loop]]
      loopGrid = concat (map mini stacks)
   in eesp (length loopGrid, length $ take desiredLengthLoops loopGrid) $ take desiredLengthLoops loopGrid
  where mini :: [a] -> [[a]]
        mini xs =
          let --cycled :: [a]
              cycled = cycle xs
              --cycles :: [[a]]
              cycles = map (\n -> drop n cycled) [0..length xs - 1]
              --firstThrees :: [[a]]
              firstThrees = map (take 3) cycles
              justOneFirstThree = [head firstThrees]
           in concat $ map oneTwoThree justOneFirstThree

groupBySourceTrack :: [Loop] -> [[Loop]]
--groupBySourceTrack = groupUsing getSourceTrackHash
groupBySourceTrack xs = eesp ("gosh", map getSourceTrackHash xs) $ groupUsing getSourceTrackHash xs

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
  msp "HEY"
  msp loopGrids
  mapM (renderLoopGrid s) loopGrids

renderLoopGrid :: State -> [[Loop]] -> IO Zound
renderLoopGrid (State { soundLoader }) loopGrid = do
  let numLoops = length (nubOrd (concat loopGrid))
  msp ("loopgrid", numLoops)
  let filenameGrid :: [[String]]
      filenameGrid = map (map loopFilename) loopGrid
      rah :: IO [[Zound]]
      rah =  mapM (mapM soundLoader) filenameGrid
  zoundGrid <- ((mapM (mapM soundLoader) filenameGrid) :: IO [[Zound]])
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

setSong :: State -> Zound -> IO (GuiCommand State)
setSong s mix = do
  mapM (msp . ("hey",)) (getAllSegments mix)
  z <- strictRender mix
  let s' = s { currentSong = Just (mix, z), currentGroup = [] }
  setZound (looper s') z
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
  ss <- loadLoopZounds (soundLoader s) (currentGroup s)
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

affinityMain :: String -> Int -> [(Double, String)] -> IO ()
affinityMain projectFile seed collections = do
  withLooper $ \looper -> do
                    soundLoader <- memoizeIO readZoundFadeEnds
                    let loader = makeLoader projectFile soundLoader looper
                    -- s <- initState projectFile soundLoader looper collections
                    let initCommands :: [GuiCommand]
                        initCommands = [Load projectFile]
                    guiMain initCommands initViz saver loader stateToViz renderViz keyboardHandler respondToStateChange 
                    --gfxMain s keyboardHandler respondToStateChange updateGfx
                    --runEditor (editor s keyboardHandler displayer respondToStateChange loader saver)
