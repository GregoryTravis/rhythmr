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

import Control.Monad (replicateM)
import qualified Data.Set as S
import Graph
import System.Random

import Loop
import Looper
import Score
import Sound
import Util

editorLogLength = 10

data State =
  State { loops :: [Loop]
        , likes :: S.Set [Loop]
        , dislikes :: S.Set [Loop]
        , currentGroup :: [Loop]
        , looper :: Looper
        , soundLoader :: String -> IO Sound
        , editorLog :: [String]
        , stack :: [[Loop]]
        , currentSong :: Maybe (Score, [[Loop]])
        , affinityCycle :: Int }

-- This is not used; it is required so that KHResults can be compared
instance Eq State where
  a == b = (likes a) == (likes b) && (dislikes a) == (dislikes b) && (currentGroup a == currentGroup b) && (editorLog a) == (editorLog b) && (stack a) == (stack b)
  -- _ == _ = undefined

instance Show State where
  show _ = "[State]"

randomGroup :: State -> IO [Loop]
randomGroup s = do
  groupSize <- getStdRandom (randomR (2,4)) :: IO Int
  replicateM groupSize (randFromList (loops s))

acceptable :: State -> [[Loop]]
acceptable = (map S.toList) . components . fromComponents . S.toList . likes

like :: State -> State
like s = nextFromStack $ s { likes = S.insert (currentGroup s) (likes s) }
dislike :: State -> State
dislike s | length (currentGroup s) < 3 = nextFromStack $ s { dislikes = S.insert (currentGroup s) (dislikes s) }
dislike s | otherwise = nextFromStack $ pushStackN s (allSubs (currentGroup s))
  where -- A sub is the list with one element removed
        allSubs :: [Loop] -> [[Loop]]
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
  where combos :: [Loop] -> [Loop] -> [[Loop]]
        combos xs ys = [xs' ++ ys' | xs' <- clump 2 xs, ys' <- clump 2 ys]

pushStack :: State -> [Loop] -> State
pushStack s x = s { stack = x : stack s }

pushStackN :: State -> [[Loop]] -> State
pushStackN s (x : xs) = pushStack (pushStackN s xs) x
pushStackN s [] = s

replaceStack :: State -> [[Loop]] -> State
replaceStack s stack_ = s { stack = stack_ }

edlog :: State -> String -> State
edlog st msg = st { editorLog = take editorLogLength (msg : editorLog st) }

renderEdLog :: State -> [String]
renderEdLog (State { editorLog = lines }) = take editorLogLength $ (reverse lines) ++ (repeat "")
