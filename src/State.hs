module State (State(..)) where

import qualified Data.Set as S

import Looper
import Sound

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

