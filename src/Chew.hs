{-# LANGUAGE NamedFieldPuns #-}

module Chew
( chew
, hiChew ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import qualified Data.Set as S
import System.Random

import Constants
import Loop
import Looper
import State
import Util
import Zounds

renderLoopGrid :: State -> [[Loop]] -> IO Zound
renderLoopGrid s loopGrid = do
  let numLoops = length (nubOrd (concat loopGrid))
  msp ("loopgrid", numLoops)
  zoundGrid <- ((mapM (mapM (loadLoopZound s)) loopGrid) :: IO [[Zound]])
  let mix :: Zound
      mix = renderGrid zoundGrid bpm
  return mix

-- Loads and resamples to standard length
loadGrid :: State -> [[Loop]] -> IO [[Zound]]
loadGrid s loopGrid = do
  let numLoops = length (nubOrd (concat loopGrid))
  msp ("loopgrid", numLoops)
  ugh <- mapM (mapM (loadLoopZound s)) loopGrid
  let ugh' :: [[Zound]]
      ugh' = map (map (Scale loopLengthFrames)) ugh
  mapM (mapM render) ugh'

renderZGrid :: [[Zound]] -> Zound
renderZGrid zses =
  let offsets = take (length zses) (map (* loopLengthFrames) [0..])
      stacks = map Mix zses
   in Mix $ zipWith Translate offsets stacks

-- Chop a time segment 0..len into n pieces and return the bounds
-- Assumes it starts at origin
slices :: Frame -> Int -> [Bounds]
slices len n =
  let s = 0
      e = len
      points :: [Frame]
      points = map floor $ map (* (fromIntegral e - fromIntegral s)) $ map ((/ fromIntegral n) . fromIntegral) [0..n]
      starts = take n points
      ends = drop 1 points
   in zipWith Bounds starts ends

-- Slices, for the length of the provided zound
slicesFor :: Zound -> Int -> [Bounds]
slicesFor z n = slices (numFrames z) n

-- Chop into n pieces and leave where they are, do not translate to origin
slice :: Int -> Zound -> [Zound]
slice n z =
  let Bounds s e = getBounds z
      points :: [Frame]
      points = map floor $ map (* (fromIntegral e - fromIntegral s)) $ map ((/ fromIntegral n) . fromIntegral) [0..n]
      starts = take n points
      ends = drop 1 points
      pieceBounds = slicesFor z n
      pieces = map (flip snipBounds z) pieceBounds
   in pieces

-- Chop into n pieces and translate to origin
sliceToOrigin:: Int -> Zound -> [Zound]
sliceToOrigin n z = map toOrigin (slice n z)

-- Chop into n pieces and translate all to origin
dice :: Int -> Zound -> [Zound]
dice n z = map toZero (slice n z)

isAtOrigin :: Zound -> Bool
isAtOrigin z = getStart (getBounds z) == 0

areAtOrigin :: [Zound] -> Bool
areAtOrigin zs = all isAtOrigin zs

-- Must be s=0
seqZounds :: [Zound] -> Zound
seqZounds zs | areAtOrigin zs =
  -- let offsets = take (length zs) $ (0 : (map getEnd $ map getBounds zs))
  --     translated = zipWith Translate offsets zs
  let translated = translateList 0 zs
   in Mix translated
  where translateList :: Int -> [Zound] -> [Zound]
        translateList dt (z : zs) = (Translate dt z) : translateList (dt + getEnd (getBounds z)) zs
        translateList dt [] = []

-- Apply a transform to the zound starts
mapStarts :: (Frame -> Frame) -> [Zound] -> [Zound]
mapStarts f zs = map update zs
  where update z = Translate dt z
          where s = getStart (getBounds z)
                dt = (f s) - s

-- stretch the start-points of the sound relative to the origin
scaleSeq :: Double -> Zound -> Zound
scaleSeq scale (Mix zs) = Mix $ mapStarts (floor . (* scale) . fromIntegral) zs

sameBounds :: (Zound -> Zound) -> (Zound -> Zound)
sameBounds f z = Bounded (getBounds z) (f z)

-- Break a the sample into n pieces, and only keep the ones mentioned in
-- 'keepers'.
chopOut :: Int -> [Int] -> Zound -> Zound
chopOut n keepers z = Mix $ zipWith keepOrSilence [0..n-1] (slice n z)
  where keepOrSilence i sz | elem i keepers = sz
        keepOrSilence i sz | otherwise = Silence (getBounds sz)
       
-- Break a the sample into n pieces, and construct a new n-piece list from the piece indices in 'pieces'.
-- -1 means a rest. If the list is too long, the extra ones are ignored. If too short, it is repeated.
sprinkle :: Int -> [Int] -> Zound -> Zound
sprinkle n indices' z =
  let pieces = sliceToOrigin n z
      places = slicesFor z n
      indices = take n $ cycle indices'
      toPiece (-1) bounds = Silence bounds
      toPiece i bounds | i < 0 || i >= length pieces = error "sprinkle.toPiece OOB"
      toPiece i bounds | otherwise = Translate (getStart bounds) (pieces !! i)
   in Mix (zipWith toPiece indices places)
       
-- chopOut n keepers z = Mix (map (pieces !!) keepers)
--   where pieces = slice n z

addClick :: Zound -> [[Zound]] -> [[Zound]]
addClick clik = map (++ [clik])

chopOuts :: [(Int, [Int])]
chopOuts =
  [ (4, [0, 2])
  , (8, [0, 2, 4, 6])
  , (4, [1, 3])
  , (8, [1, 3, 5, 7])
  , (4, [0, 3])
  , (8, [0, 5])
  , (8, [3, 7, 8])
  ]

sprinklers :: [(Int, [Int])]
sprinklers =
  [ (4, [0, 1])
  , (32, [3, 2, 3, 2, 8, 5, 8, 5])
  , (32, [0, 0, 0, 1, 1, 1, 4, 4, 4, 13, 13, 13])
  , (16, [0, 4, 11])
  , (16, [0, 1, 2])
  , (8, [0, 1, 2])
  , (4, [1, 1, 0, 1])
  , (8, [0, 7, 2, 7, 4, 7, 6, 7])
  ]

-- Use the first loop intact; chop out the rest
wackyStack :: [Zound] -> Zound
wackyStack [] = error "empty wackyStack"
wackyStack (z:zs) = Mix $ [z] ++ zipWith co (cycle chopOuts) zs
  --where co (n, keepers) z = chopOut n keepers z
  where co (n, keepers) z = sprinkle n keepers z

-- Rotate the stack a few times
wackyStacks :: [Zound] -> [Zound]
wackyStacks zs = map wackyStack $ map (flip rotate zs) [0..n-1]
  where n = 3

-- Pick a spot in the list to insert an element, and remove one from the end
shunt :: Int -> a -> [a] -> [a]
shunt i e xs = init $ (take i xs) ++ [e] ++ (drop i xs)

randParam :: (Int, Int) -> (Int -> f) -> [f]
randParam range f = zipWith ($) (repeat f) rands
  where rands = randomRs range (mkStdGen 37)

randParam2 :: (Int, Int) -> (Int -> Int -> f) -> [f]
randParam2 range f = zipWith ($) (zipWith ($) (repeat f) rands) rands'
  where rands = randomRs range (mkStdGen 37)
        rands' = randomRs range (mkStdGen 2036)

-- Apply first function to value, pass result to second, etc, and return all
-- values (including the initial one)
runThrough :: [a -> a] -> a -> [a]
runThrough [] _ = []
runThrough (f:fs) x = x : (runThrough fs (f x))

-- Return elements at odd positions
odds :: [a] -> [a]
odds xs = odds' False xs
  where odds' True (x:xs) = x : (odds' False xs)
        odds' False (x:xs) = odds' True xs

-- Shunt and shunt again. 'odds' makes we do two shunts per step
shuntMadness :: [Int] -> [[Int]]
shuntMadness xs = odds $ runThrough randShunt xs
  where randShunt = randParam2 (0, length xs-1) shunt

dnb :: State -> IO Zound
dnb s = do
  -- z <- readZound "hey.wav" >>= yah
  -- z2 <- readZound "hay.wav" >>= yah
  --msp ("likes", S.toList $ likes s)
  --msp ("likes", head $ S.toList $ likes s)
  let [zf, z2f, z3f] = twoOrThree $ head $ S.toList $ likes s
  --msp ("um", zf, z2f, z3f)
  [z', z2', z3'] <- mapM (loadLoopZound s) [zf, z2f, z3f]
  --msp ("um", z', z2', z3')
  [z'', z2'', z3''] <- mapM render [z', z2', z3']
  --msp ("um", z'', z2'', z3'')
  [z, z2, z3] <- mapM yah [z'', z2'', z3'']
  --msp ("um", z, z2)
  let shunts = take 10 $ shuntMadness [0..15]
      shunteds = map (\s -> sprinkle 16 s z) shunts
      grid = map (:[z2, z3]) shunteds
      score = renderZGrid grid
  return score
  where yah z = render (Scale loopLengthFrames z)
        twoOrThree (x:y:z:_) = [x, y, z]
        twoOrThree [x, y] = [x, y, y]

hiChew :: State -> IO Zound
hiChew s = do
  z <- readZound "one.wav" >>= yah
  z' <- readZound "two.wav" >>= yah
  let s = sprinkle 4 [0, -1, 2, -1] z
      s' = sprinkle 4 [-1, 1, -1, 3] z'
  let grid = [[z], [z'], [s], [s'], [s, s']]
      score = renderZGrid grid
  return score
  where yah z = render (Scale loopLengthFrames z)

chew = dnb
_chew s = do
  clik <- readZound "wavs/clik.wav"
  --let loops = S.toList (likes s)
  let loops = S.toList $ likes s
  likes' <- loadGrid s loops
  let likes = reverse $ sortOn length likes'
  let grid = map (:[]) $ concat (map wackyStacks likes)
  let song = renderZGrid $ {-addClick clik-} grid
  mix <- time "zrender" $ strictRender song
  writeZound "chew.wav" mix
  return mix
