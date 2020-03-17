module Viz
  ( stateToViz'
  , initViz
  , renderViz'
  ) where

import qualified Data.Map.Strict as M
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Linear
import System.Exit (exitSuccess)

import Animate
import Gui
import Loop
import State
import Util

type ID = String

data Viz = Viz (AValMap String (V2 Float))

gridSizeFor :: Int -> Int
gridSizeFor n = ceiling $ sqrt $ fromIntegral n

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

initViz :: Viz
initViz = Viz (emptyAValMap (Interpolator interpV))

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

stateToViz' :: Viz -> State -> Float -> Viz
stateToViz' (Viz aValMap) s t = Viz aValMap'
  where aValMap' = gcAValMap t $ foldr set aValMap (stateToPositions s)
        set (id, pos) avm = setAVal t id pos avm
--setAVal :: Ord k => Float -> k -> a -> AValMap k a -> AValMap k a
--foldr :: (a -> b -> b) -> b -> t a -> b
--setAVal :: Ord k => Float -> k -> a -> AValMap k a -> AValMap k a

stateToPositions :: State -> [(String, V2 Float)]
stateToPositions s =
  zip loopNames $ map (\k -> M.findWithDefault def k positions) (loops s) -- [0..length (loops s) - 1]
    where positions = affinityPositions s
          def = V2 0 0
          loopNames = map (\(Loop loopName) -> loopName) (loops s)

renderViz' :: Float -> Viz -> (Picture, Viz)
renderViz' t (Viz avm) = (Pictures (map render vals), (Viz avm'))
  where render (_, (V2 x y)) = Translate x y $ Circle 10
        (vals, avm') = getAllAVals avm t
