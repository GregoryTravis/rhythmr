{-# LANGUAGE NamedFieldPuns #-}

module Gui
  ( guiMain
  , GuiCommand(..)
  , windowWidth
  , windowHeight
  , windowDim ) where

import qualified Data.Set as S
import Control.Concurrent (forkIO, threadDelay, killThread)
--import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (finally)
import Control.Monad.STM (atomically)
import Data.Binary
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime, SystemTime)
--import GHC.Float (float2Float)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Linear
import System.Exit (exitSuccess)
import System.Random

import History
import SaveLoad
import State
import Util

windowWidth = 800
windowHeight = 800
windowDim = V2 windowWidth windowHeight

data GuiState s v = GuiState (History s) Float v

-- lol "Save String"
data GuiCommand s = NewState s | Save String | Load String | Undo | Redo | Quit | DoNothing
  deriving Show

guiMain :: (Eq s, Show s, Read t, Show t, Binary t) => [GuiCommand s] -> s -> v -> Saver s t -> Loader s t -> (s -> v -> s -> Float -> v) -> (Float -> s -> v -> IO Picture) ->
                                             (s -> Char -> IO (GuiCommand s)) -> (s -> s -> IO ()) -> IO ()
guiMain initCommands s initViz saver loader stateToViz renderViz keyboardHandler respondToStateChange =
  let initWorld = GuiState (start s) 0 (stateToViz s initViz s 0)
      worldToPicture (GuiState h t v) = renderViz t (cur h) v
      eventHandler (EventKey (SpecialKey KeyEsc) Down x y) gs = eventHandler (EventKey (Char '\ESC') Down x y) gs
      eventHandler (EventKey (SpecialKey KeySpace) Down x y) gs = eventHandler (EventKey (Char ' ') Down x y) gs
      -- This is extremely goofy of me
      eventHandler (EventKey (SpecialKey KeyLeft) Down x y) gs = eventHandler (EventKey (Char '\STX') Down x y) gs
      eventHandler (EventKey (SpecialKey KeyRight) Down x y) gs = eventHandler (EventKey (Char '\ETX') Down x y) gs
      eventHandler (EventKey (Char c) Down _ _) gs@(GuiState h t v) = do
        command <- keyboardHandler (cur h) c
        --msp command
        h' <- execute command h saver loader
        --msp ("boing", length (currentGroup (cur h)), length (currentGroup (cur h')))
        if h == h' && (cur h) == (cur h')
           then return gs
           else do respondToStateChange (cur h) (cur h')
                   return $ GuiState h' t (stateToViz (cur h) v (cur h') t)
      eventHandler e gs = do
        --msp $ "?? " ++ (show e)
        return gs
      stepIteration dt (GuiState h t v) = return $ GuiState h (t + dt) v
   in playIO displayMode bgColor 100 initWorld worldToPicture eventHandler stepIteration
  where displayMode = InWindow "Rhythmr" (windowWidth, windowHeight) (810, 10)
        bgColor = white

-- Loading a history with 169 states -- 1.5M on disk -- pins all four cpus at
-- 100%. I don't know why. Further investigation was even more baffling.
--
-- It sort of seems like
--   (1) that's too much state for a program with a 93M heap
--   (2) the heap is not growing
--   (3) it's GC'ing like mad
loadHistoryAndSurviveSomehow :: (Show s, Read t, Show t, Binary t) => FilePath -> Loader s t -> IO (History s)
loadHistoryAndSurviveSomehow filename loader = do
  -- Normal: pins
  -- load filename loader

  h <- load filename loader
  msp ("len", length (toList h))

  -- wait this one is good
  -- let [s0, s1] = take 2 (toList h)
  --     h' = fromList [s0, s1]

  -- and this one is bad?
  -- let h' = fromList (take 2 (toList h))

  -- and this is bad too?
  -- let ss = take 2 (toList h)
  --     h' = fromList ss

  -- But this one is okay??
  -- Is this because History/Zipper put the first one in a different place?
  -- let ss = take 1 (toList h)
  --     h' = fromList ss

  -- but this is ok just because we print it?
  -- works for: 2, 10, 20
  -- a little higher but still not pinning: 30, 40, 60
  -- even higher but not pinning: 100
  -- almost pinned, but not pinned: 120!
  -- let ss = take 2 (toList h)
  --     h' = fromList ss
  -- msp h'

  -- aaaand guess what, if we add this flag to the cabal file, we don't have to do anything special:
  -- ghc-options:
  --   -with-rtsopts=-H128m
  let h' = h

  -- This works if you evaluate the unevaluated tail of the list
  -- Presumably the thunk is holding on to the entire ist
  -- let h' = fromList (take 2 (toList h))
  -- msp $ drop 2 (toList h')

  return h'

  -- Grab some: pins iff more than one state
  -- h <- load filename loader
  -- msp ("len", length (toList h))
  -- let h' = fromList (take 1 (toList h))
  -- return h'

  -- Throw away giant history, fixes cpu pinning
  -- Load filename -> do fmap (start . cur) $ load filename loader

execute :: (Show s, Read t, Show t, Binary t) => GuiCommand s -> History s -> Saver s t -> Loader s t -> IO (History s)
execute command h saver loader =
  case command of NewState s -> return $ update h s
                  Save filename -> do save filename saver h
                                      return h
                  -- Load filename -> load filename loader
                  Load filename -> loadHistoryAndSurviveSomehow filename loader
                  -- Load filename -> do h <- load filename loader
                  --                     msp ("len", length (toList h))
                  --                     let h' = fromList (take 1 (toList h))
                  --                     return h'
                  Undo -> return $ undo h
                  Redo -> return $ redo h
                  Quit -> exitSuccess
