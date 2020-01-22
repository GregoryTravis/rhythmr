module Score
( Score(..)
, Measure(..)
, renderScore ) where

import Arrangement
import Constants
import Data.List (nub)
import qualified Data.Map as M
import FX
import Sound
import Util

data Measure = Measure Int FX
data Score = Score [[Measure]]


  --     arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleSoundArrangement loopLengthFrames) ss)) accSounds
renderScore :: Score -> [Sound] -> IO Arrangement
renderScore (Score mses) sounds = do
  let ns = nub $ map measureInt (concat mses)
      nsmap = M.fromList (zip ns (cycle sounds))
      m2ssa :: Measure -> IO Arrangement
      m2ssa (Measure i fx) = do
        let sound = nsmap M.! i
        sound' <- applyFX fx sound
        return (singleSoundArrangement loopLengthFrames sound')
  measureses <- mapM (\st -> mapM m2ssa st) mses
  return $ seqArrangement (map parArrangement measureses)

measureInt (Measure n _) = n
