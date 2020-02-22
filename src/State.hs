module State
  ( State(..)
  , randomGroup
  , acceptable
  , like
  , dislike
  , pushCurrentGroup
  , nextFromStack
  , combineAffinities
  , pushStack
  , pushStackN
  , edlog
  , renderEdLog
  ) where

import qualified Data.Set as S
import Graph
import System.Random

import Looper
import Sound
import Util

editorLogLength = 10

data State =
  State { sounds :: [Sound]
        , likes :: S.Set [Int]
        , dislikes :: S.Set [Int]
        , currentGroup :: [Int]
        , looper :: Looper
        , editorLog :: [String]
        , stack :: [[Int]] }

instance Eq State where
  _ == _ = undefined

instance Show State where
  show = undefined

randomGroup :: State -> IO [Int]
randomGroup s = do
  let soundIndices = [0..length (sounds s)-1]
  groupSize <- getStdRandom (randomR (2,4)) :: IO Int
  indices <- mapM (\_ -> randFromList soundIndices) [0..groupSize-1]
  return indices

acceptable :: State -> [[Int]]
acceptable = (map S.toList) . components . fromComponents . S.toList . likes

like :: State -> State
like s = nextFromStack $ s { likes = S.insert (currentGroup s) (likes s) }
dislike :: State -> State
dislike s | length (currentGroup s) < 3 = nextFromStack $ s { dislikes = S.insert (currentGroup s) (dislikes s) }
dislike s | otherwise = nextFromStack $ pushStackN s (allSubs (currentGroup s))
  where -- A sub is the list with one element removed
        allSubs :: [Int] -> [[Int]]
        allSubs (x : xs) = [xs] ++ map (x:) (allSubs xs)
        allSubs [] = []

pushCurrentGroup :: State -> State
pushCurrentGroup s = s { stack = map p2l $ allPairs (currentGroup s) }
  where p2l (x, y) = [x, y]

nextFromStack :: State -> State
nextFromStack s | (stack s) /= [] = s { currentGroup = g, stack = gs }
                | otherwise = s
  where (g:gs) = stack s

combineAffinities :: State -> State
combineAffinities s =
  case acceptable s of (a : b : _) -> nextFromStack $ replaceStack s (combos a b)
                       _ -> s
  where combos :: [Int] -> [Int] -> [[Int]]
        combos xs ys = [xs' ++ ys' | xs' <- clump 2 xs, ys' <- clump 2 ys]

pushStack :: State -> [Int] -> State
pushStack s x = s { stack = x : stack s }

pushStackN :: State -> [[Int]] -> State
pushStackN s (x : xs) = pushStack (pushStackN s xs) x
pushStackN s [] = s

replaceStack :: State -> [[Int]] -> State
replaceStack s stack_ = s { stack = stack_ }

edlog :: State -> String -> State
edlog st msg = st { editorLog = take editorLogLength (msg : editorLog st) }

renderEdLog :: State -> [String]
renderEdLog (State { editorLog = lines }) = take editorLogLength $ (reverse lines) ++ (repeat "")
