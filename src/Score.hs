module Score
( Score(..)
, Measure(..)
, renderScore ) where

import Arrangement
import Constants
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as M
import FX
import Sound
import Util

data Measure = Measure (Int, Int) FX
  deriving (Eq, Show)
data Score = Score [[Measure]]
  deriving (Eq, Show)

  --     arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleSoundArrangement loopLengthFrames) ss)) accSounds
renderScore :: Score -> [[Sound]] -> IO Arrangement
renderScore s@(Score mses) sounds = do
  let m2ssa :: Measure -> IO Arrangement
      m2ssa (Measure (g, i) fx) = do
        let sound = eesp (g, length sounds, i, length (sounds !! g)) ((sounds !! g) !! i)
        sound' <- applyFX fx sound
        return (singleSoundArrangement loopLengthFrames sound')
  measureses <- mapM (\st -> mapM m2ssa st) mses
  return $ seqArrangement (map parArrangement measureses)

-- -- Separate scores, one for each (Measure (g, i) _) (don't distinguish by fx)
-- splitScoreByMeasure :: Score -> [Score]
-- splitScoreByMeasure score = map (justThisMeasure score) (allMeasures score)

-- allMeasures :: Score -> [(Int, Int)]
-- allMeasures (Score measures) = nubOrd $ map getCoords measures
--   where getCoords (Measure coords _) = coords

-- -- Replace any measure other than the specified one with silence
-- justThisMeasure :: (Int, Int) -> Score -> Score
-- justThisMeasure coords (Score measures) = Score (replaceIfDifferent measures)
--   where replaceIfDifferent m@(Measure coords' _) | coords == coords' = m
--         replaceIfDifferent m@(Measure coords fx) = 
