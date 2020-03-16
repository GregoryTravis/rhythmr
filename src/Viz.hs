module Viz
  ( statesToViz'
  , renderViz'
  , updateViz
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

data Ding = Ding (V2 Float) (V2 Float)
  deriving Show

data Viz = Viz [Ding]

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

-- updateGfx :: GuiState -> GuiState
-- updateGfx gs = gs { getDings = newDings }
--   where newDings = zipWith (\x d -> Ding x d) xs $ map (\k -> M.findWithDefault def k positions) [0..length (sounds (getState gs)) - 1]
--         def = V2 0 0
--         positions = affinityPositions (getState gs)
--         xs = map (\(Ding x d) -> x) (getDings gs)

statesToViz' :: State -> State -> Viz
statesToViz' s s' = Viz $ zipWith Ding (stateToPositions s) (stateToPositions s')

stateToPositions :: State -> [V2 Float]
stateToPositions s =
  map (\k -> M.findWithDefault def k positions) (loops s) -- [0..length (loops s) - 1]
    where positions = affinityPositions s
          def = V2 0 0

updateViz :: Float -> Viz -> Viz
updateViz dt (Viz dings) = Viz newDings
  where newDings = map update dings
        update :: Ding -> Ding
        update (Ding x d) = Ding (x + clip (d-x)) d
          where clip v | norm v > vel = vel *^ signorm v
                clip v | otherwise = v
                vel = 1000.0 * dt

renderViz' :: Viz -> Picture
renderViz' (Viz dings) = Pictures (map render dings)
  where render (Ding (V2 x y) _) = Translate x y $ Circle 10
