module Score
( Score(..)
, Measure(..)
, renderScore
, allInts ) where

import Arrangement
import Constants
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as M
import FX
import Sound
import Util

data Measure = Measure Int FX
  deriving (Eq, Show)
data Score = Score [[Measure]]
  deriving (Eq, Show)

  --     arr = seqArrangement $ map dub $ map (\ss -> parArrangement (map (singleSoundArrangement loopLengthFrames) ss)) accSounds
renderScore :: Score -> [Sound] -> IO Arrangement
renderScore s@(Score mses) sounds = do
  let ns = allInts s
      nsmap = M.fromList (zip ns (cycle sounds))
      m2ssa :: Measure -> IO Arrangement
      m2ssa (Measure i fx) = do
        let sound = nsmap M.! i
        sound' <- applyFX fx sound
        return (singleSoundArrangement loopLengthFrames sound')
  measureses <- mapM (\st -> mapM m2ssa st) mses
  return $ seqArrangement (map parArrangement measureses)

measureInt (Measure n _) = n

allInts :: Score -> [Int]
allInts (Score measureses) = nubOrd $ map measureInt $ concat measureses
