{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Viz
  ( stateToViz
  , initViz
  , renderViz
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
--import Graphics.Gloss.Data.Picture
--import Graphics.Gloss.Interface.IO.Game
import Linear
import System.Exit (exitSuccess)

import Animate
import Gui
import Loop
import Score
import State
import Util

duration = 0.5

gridSizeFor :: Int -> Int
gridSizeFor n = ceiling $ sqrt $ fromIntegral n

-- What are the coordinates of i in a sqaure-ish grid of n values?
toGridXY :: Int -> Int -> V2 Int
toGridXY i n = V2 x y
  where gridSize = gridSizeFor n
        x = i `mod` gridSize
        y = i `div` gridSize

toGridXYF i n = (fmap fromIntegral (toGridXY i n)) / (fmap fromIntegral (V2 gridSize gridSize))
  where gridSize = gridSizeFor n

-- unitSquareTo :: V2 Float -> V2 Float -> (Picture -> Picture)
-- unitSquareTo (V2 llx lly) (V2 w h) picture = Translate llx lly $ Scale w h picture

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
        margin = 0.65

affinityPositions :: State -> M.Map Loop (V2 Float)
affinityPositions s = case acceptable s of xss -> M.fromList $ concat (zipWith rah (gridTransformsForN (length xss)) xss)
  where rah :: (V2 Float -> V2 Float) -> [Loop] -> [(Loop, V2 Float)]
        rah xform xs = zip xs $ map (\cXform -> ((scaler (V2 400 400)) . xform . cXform) (V2 0 0)) (ringOfCirclesInUnitSquare (length xs))

interpV :: Float -> Float -> Float -> V2 Float -> V2 Float -> V2 Float
interpV t s e (V2 x y) (V2 x' y') = V2 x'' y''
  where x'' = interp t s e x x'
        y'' = interp t s e y y'

interp :: Float -> Float -> Float -> Float -> Float -> Float
--interp t s e a a' = eesp (t, s, e, a, a') $ a + (k * (a' - a))
interp t s e a a' = a + (k * (a' - a))
  where k = clip 0.0 1.0 ((t - s) / (e - s))

clip :: (Ord a, Num a) => a -> a -> a -> a
clip lo hi x | x < lo = lo
clip lo hi x | x > hi = hi
clip lo hi x | otherwise = x

-- Some potential for inconsistency here, Pic and it's Tag could differ
data Tag = LoopT Loop | SeqT Loop Int Float
  deriving (Eq, Show, Ord)
data Pic c = LoopP Tag (c (V2 Float))
           | SeqP Tag (c (V2 Float)) (c Float)
deriving instance () => Show (Pic AVal)
deriving instance () => Show (Pic Id)

data Id a = Id a
  deriving Show

mapPic :: (forall a . (Eq a, Show a) => c a -> c' a) -> Pic c -> Pic c'
mapPic f (SeqP tag pos size) = SeqP tag (f pos) (f size)
mapPic f (LoopP tag pos) = LoopP tag (f pos)

zipWithPic :: (forall a . (Eq a, Show a) => c a -> d a -> e a) -> Pic c -> Pic d -> Pic e
zipWithPic f (SeqP tag pos size) (SeqP tag' pos' size') | tag == tag' = SeqP tag (f pos pos') (f size size')
zipWithPic f (LoopP tag pos) (LoopP tag' pos') | tag == tag' = LoopP tag (f pos pos')

mapSquish :: (forall a . (Eq a, Show a) => c a -> b) -> Pic c -> [b]
mapSquish f (SeqP tag pos size) = [f pos, f size]
mapSquish f (LoopP tag pos) = [f pos]

gcReport :: Viz -> [Int]
gcReport (Viz pics) = concat $ map (mapSquish aValSize) pics

getTag :: Pic a -> Tag
getTag (LoopP tag _) = tag
getTag (SeqP tag _ _) = tag

picInterpolator :: Pic c -> Pic Interpolator
picInterpolator (LoopP tag _) =
  LoopP tag v2FloatInterpolator
picInterpolator (SeqP tag _ _) =
  SeqP tag v2FloatInterpolator floatInterpolator

-- Surely this exists already
data Pair c d a = Pair (c a) (d a)
pair :: (c a) -> (d a) -> Pair c d a
pair = Pair

updatePic :: Float -> Float -> Pic AVal -> Pic AVal -> Pic AVal
updatePic t t' old new = mapPic f $ zipWithPic pair (zipWithPic pair old new) (picInterpolator new)
  where f :: (Eq a, Show a) => (Pair (Pair AVal AVal) Interpolator) a -> AVal a
        f (Pair (Pair aval v) interpolator) = updateAVal t t' aval v interpolator

idToAVal :: Id a -> AVal a
idToAVal (Id a) = constAVal a
aValToId :: Show a => Float -> AVal a -> Id a
aValToId t = Id . flip readAVal t

constPic :: Pic Id -> Pic AVal
constPic = mapPic idToAVal

v2FloatInterpolator = Interpolator interpV
floatInterpolator = Interpolator interp
colorInterpolator = Interpolator colorInterpolator'
colorInterpolator' :: Float -> Float -> Float -> Color -> Color -> Color
colorInterpolator' t s e color color' = makeColor r'' g'' b'' a''
  where r'' = interp t s e r r'
        g'' = interp t s e g g'
        b'' = interp t s e b b'
        a'' = interp t s e a a'
        (r, g, b, a) = rgbaOfColor color
        (r', g', b', a') = rgbaOfColor color'

-- pef :: Show (c _) => Pic c
-- pef = undefined

data Viz = Viz [Pic AVal]
  deriving Show
initViz :: Viz
initViz = Viz []

-- Match old and new Pics via id; new ones are just initialized via const
updateViz :: Float -> Viz -> [Pic AVal] -> Viz
updateViz t (Viz oldPics) newPics =
  let oldAndNew = filter hasNew $ pairUp oldPics newPics getTag getTag
   in Viz $ map merge oldAndNew
  where hasNew (_, Nothing) = False
        hasNew _ = True
        merge (Just oldPic, Just newPic) = updatePic t (t+duration) oldPic newPic
        merge (Nothing, Just newPic) = newPic

renderViz :: Float -> Viz -> Picture
renderViz t (Viz pics) = {-whatThread "renderViz" $-} Pictures $ map renderPic (map (mapPic (aValToId t)) pics)

loopColor :: Loop -> Color
loopColor loop =
  let hash = getHash loop
      ri = read ("0x" ++ (take 2 hash)) :: Int
      gi = read ("0x" ++ (take 2 $ drop 2 hash)) :: Int
      bi = read ("0x" ++ (take 2 $ drop 4 hash)) :: Int
      r = fromIntegral ri / 256.0
      g = fromIntegral gi / 256.0
      b = fromIntegral bi / 256.0
   in makeColor r g b 1.0

rect :: Picture
rect = Polygon $ rectanglePath 25.0 20.0

renderPic :: Pic Id -> Picture
renderPic (LoopP (LoopT loop) (Id (V2 x y))) = Translate x y $ Color color $ rect
  where color = loopColor loop
renderPic (SeqP (SeqT loop _ _) (Id (V2 x y)) (Id size)) = Translate x y $ Color color $ rect
  where color = loopColor loop

stateToPics :: Float -> State -> State -> [Pic AVal]
stateToPics t oldS s = affinitiesToPics s ++ sequenceToPics t oldS s

affinitiesToPics :: State -> [Pic AVal]
affinitiesToPics s@(State { loops }) = map toPic loops
  where toPic loop = constPic $ LoopP (LoopT loop) (Id pos)
          where (pos, color) = case aps M.!? loop of Just pos -> (pos, red)
                                                     Nothing -> ((gridPosition loop s), green)
                aps = affinityPositions s

sequenceToPics :: Float -> State -> State -> [Pic AVal]
sequenceToPics t _ (State { currentSong = Nothing }) = []
sequenceToPics t oldS s =
  let State { currentSong = Just (score, loops) } = s
      theSame = currentSong oldS == currentSong s
   in L.zipWith (toPic theSame) [0..] (endPositions score loops)
  where toPic theSame i (loop, pos) = if (esp theSame) then constPic endPic else combine startPic endPic
          where endPic = SeqP (SeqT loop i t) (Id pos) (Id 10.0)
                startPic = SeqP (SeqT loop i t) (Id startPos) (Id 10.0)
                startPos = aps M.! loop
        combine p p' = updatePic (t+2*duration) (t+3*duration) (constPic p) (constPic p')
        endPositions score loops = (seqLayOutPositions $ seqLoopsAndPositions score loops)
        aps = affinityPositions s

seqLayOutPositions :: [(Loop, V2 Int)] -> [(Loop, V2 Float)]
seqLayOutPositions poses = map lop poses
  where lop (loop, pos) = (loop, fpos pos)
        fpos pos = fmap fromIntegral pos * room - (window / 2.0) + seqMargin
        window = fmap fromIntegral $ V2 windowWidth windowHeight
        room :: V2 Float
        room = V2 28.0 22.0
        allXs :: [Int]
        allXs = map (\(_, V2 x _) -> x) poses
        allYs :: [Int]
        allYs = map (\(_, V2 _ y) -> y) poses
        seqSizeI :: V2 Int
        seqSizeI = V2 (maximum allXs) (maximum allYs)
        seqSize :: V2 Float
        seqSize = fmap fromIntegral seqSizeI * room
        seqMargin :: V2 Float
        seqMargin = (seqWindow - seqSize) / 2
        seqWindow :: V2 Float
        seqWindow = window / 2 -- V2 (windowWidth `div` 2) (windowHeight `div` 2)

seqLoopsAndPositions :: Score -> [[Loop]] -> [(Loop, V2 Int)]
seqLoopsAndPositions (Score measureses) loops = concat $ zipWith col measureses [0..]
  where col :: [Measure] -> Int -> [(Loop, V2 Int)]
        col measures x = zipWith (one x) measures [0..]
        one :: Int -> Measure -> Int -> (Loop, V2 Int)
        one x (Measure (g, i) _) y = ((loops !! g) !! i, V2 x y)

reportViz :: Viz -> Viz
reportViz = id
--reportViz v = eesp (gcReport v) v

stateToViz :: State -> Viz -> State -> Float -> Viz
stateToViz oldS v s t = reportViz $ updateViz t v (stateToPics t oldS s)

gridPosition :: Loop -> State -> V2 Float
gridPosition loop (State { loops }) =
  let i = fromJust $ loop `L.elemIndex` loops
      xflip (V2 x y) = V2 (-x) y
      window = V2 windowWidth windowHeight
      subWindow = (fmap fromIntegral window) / 2.0 - margin * 2
      margin = pure $ fromIntegral $ (min windowWidth windowHeight) `div` 15
   in xflip $ subWindow * toGridXYF i (length loops) + margin
