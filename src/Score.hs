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
        let sound = ((sounds !! g) !! i)
        sound' <- applyFX fx sound
        return (singleSoundArrangement loopLengthFrames sound')
  measureses <- mapM (\st -> mapM m2ssa st) mses
  return $ seqArrangement (map parArrangement measureses)
