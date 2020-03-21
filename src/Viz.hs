{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Viz
  ( stateToViz'
  , initViz
  , renderViz'
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
--import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import Linear
import System.Exit (exitSuccess)

import Animate
import Gui
import Loop
import State
import Util

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
data Tag = LoopT Loop | SeqT Loop Int
  deriving (Eq, Show, Ord)
data Pic c = LoopP Tag (c (V2 Float)) (c Color)
           | SeqP Tag (c (V2 Float)) (c Float) (c Color)
  --deriving Show
---deriving instance Show a => Show (Pic (c :: a -> c a))
deriving instance () => Show (Pic AVal)

data Id a = Id a
  deriving Show

getTag :: Pic a -> Tag
getTag (LoopP tag _ _) = tag
getTag (SeqP tag _ _ _) = tag

updatePic :: Float -> Pic AVal -> Pic Id -> Pic AVal
updatePic t (LoopP tag pos color) (LoopP tag' (Id pos') (Id color')) | tag == tag' =
  LoopP tag (updateAVal t pos pos' v2FloatInterpolator)
            (updateAVal t color color' colorInterpolator)
updatePic t (SeqP tag pos size color) (SeqP tag' (Id pos') (Id size') (Id color')) | tag == tag' =
  SeqP tag (updateAVal t pos pos' v2FloatInterpolator)
           (updateAVal t size size' floatInterpolator)
           (updateAVal t color color' colorInterpolator)

constPic :: Pic Id -> Pic AVal
constPic (LoopP tag (Id pos) (Id color)) = LoopP tag (constAVal pos) (constAVal color)
constPic (SeqP tag (Id pos) (Id size) (Id color)) = SeqP tag (constAVal pos) (constAVal size) (constAVal color)

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

--mapOverVals :: forall a c c' . (c a -> c' a) -> Pic c -> Pic c'
--mapOverVals :: (c a -> c' a) -> Pic c -> Pic c'
mapOverVals :: (forall a . Show a => c a -> c' a) -> Pic c -> Pic c'
mapOverVals f (SeqP tag pos size color) = SeqP tag (f pos) (f size) (f color)
mapOverVals f (LoopP tag pos color) = LoopP tag (f pos) (f color)
-- This fails because f might have any single particular value for it's a, but we are here applying it to Int
-- No, actually, RankNTypes fixes this, I just had the forall in the wrong place

initty :: Id a -> AVal a
initty (Id a) = constAVal a

data Viz = Viz [Pic AVal]
  deriving Show
initViz :: Viz
initViz = Viz []

-- Match old and new Pics via id; new ones are just initialized via const
updateViz :: Float -> Viz -> [Pic Id] -> Viz
updateViz t (Viz oldPics) newPics =
  let oldTagToPic :: M.Map Tag (Pic AVal)
      oldTagToPic = M.fromList (zip (map getTag oldPics) oldPics)
      newTagToPic :: M.Map Tag (Pic Id)
      newTagToPic = M.fromList (zip (map getTag newPics) newPics)
      -- For each new Pic, get it's corresponding old one, if any
      newAValPics :: [Pic AVal]
      newAValPics = map interp (M.toList newTagToPic)
      interp :: (Tag, Pic Id) -> Pic AVal
      interp (id, newPic) = case oldTagToPic M.!? id of Just (oldAVal) -> updatePic t oldAVal newPic
                                                        Nothing -> constPic newPic
   in Viz newAValPics

renderViz :: Float -> Viz -> Picture
renderViz t (Viz pics) = Pictures $ map (renderPic t) pics

renderPic :: Float -> Pic AVal -> Picture
renderPic t (LoopP _ pos color) =
  let (V2 x y) = readSingleAVal pos t
      color' = readSingleAVal color t
   in Translate x y $ Color color' $ Circle 10
renderPic t (SeqP _ pos size color) =
  let (V2 x y) = readSingleAVal pos t
      size' = readSingleAVal size t
      color' = readSingleAVal color t
   in Translate x y $ Color color' $ Circle size'

welp :: String
welp =
  let pics :: [Pic Id]
      pics = [LoopP (LoopT (Loop "asdf")) (Id (V2 0.0 0.0)) (Id (makeColor 1.0 0.0 0.0 1.0))]
      pics' :: [Pic Id]
      pics' = [LoopP (LoopT (Loop "asdf")) (Id (V2 1.0 2.0)) (Id (makeColor 0.0 1.0 0.0 1.0))]
      pics'' :: [Pic Id]
      pics'' = [LoopP (LoopT (Loop "asdf")) (Id (V2 4.0 4.0)) (Id (makeColor 0.0 1.0 0.0 1.0))]
      wiz :: Viz
      wiz =  updateViz 0.0 initViz pics
      wiz' :: Viz
      wiz' =  updateViz 1.0 wiz pics'
      wiz'' :: Viz
      wiz'' =  updateViz 1.5 wiz' pics''
      --leg = show $ (wiz', renderViz 1.5 wiz')
      leg = show $ [renderViz t wiz'' | t <- [1.0, 1.5, 1.75, 2.0, 2.5]]
      --leg = show wiz'
      --leg = 34
   in leg

stateToPics :: State -> [Pic Id]
stateToPics s@(State { loops }) = map toPic loops
  where toPic loop = LoopP (LoopT loop) (Id pos) (Id color)
          where (pos, color) = case aps M.!? loop of Just pos -> (pos, red)
                                                     Nothing -> ((gridPosition loop s), green)
                aps = affinityPositions s

--stateToPositions :: State -> [(Loop, V2 Float)]
--stateToPositions s =
--  zip (loops s) $ map lookup (loops s)
--    where positions = esp $ affinityPositions s
--          lookup loop = M.findWithDefault (gridPos loop) loop positions
--          color loop = if M.member loop positions then green else red
--          gridPos loop = gridPosition loop s

--stateToPics :: State -> [Pic Id]
--stateToPics s = map toPic (stateToPositions s)
--  where toPic (loop, pos) = LoopP (LoopT loop) (Id pos) (Id red)
----data Pic c = LoopP Tag (c (V2 Float)) (c Color)
----data Tag = LoopT Loop | SeqT Loop Int

stateToViz' :: Viz -> State -> Float -> Viz
stateToViz' v s t = updateViz t v (stateToPics s)

gridPosition :: Loop -> State -> V2 Float
gridPosition loop (State { loops }) =
  let i = fromJust $ loop `L.elemIndex` loops
      xflip (V2 x y) = V2 (-x) y
      window = V2 windowWidth windowHeight
      subWindow = (fmap fromIntegral window) / 2.0 - margin * 2
      margin = pure $ fromIntegral $ (min windowWidth windowHeight) `div` 15
   in xflip $ subWindow * toGridXYF i (length loops) + margin

renderViz' :: Float -> Viz -> Picture
renderViz' = renderViz
