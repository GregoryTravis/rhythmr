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
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import System.Random

import History
import SaveLoad
import State
import Util

windowWidth = 800
windowHeight = 800
windowDim = V2 windowWidth windowHeight

data GuiState s v = GuiState { history :: History s, now :: Float, viz :: v, lastSave :: History s }

-- lol "Save String"
data GuiCommand s = NewState s | Save FilePath | Load FilePath | Undo | Redo | UndoFully | RedoFully | Quit | QuitWithoutSaving | SaveAndQuit FilePath | GuiCommands [GuiCommand s] | DoNothing
  deriving Show

guiMain :: (Eq s, Show s, Read t, Show t, Binary t) => s -> Maybe FilePath -> v -> Saver (History s) t -> Loader (History s) t -> (s -> v -> s -> Float -> v) -> (Float -> s -> v -> v) -> (Float -> s -> v -> IO Picture) ->
                                             (s -> (Key, Modifiers) -> IO (GuiCommand s)) -> (s -> s -> IO ()) -> IO () -> IO ()
guiMain defaultState filenameMaybe initViz' saver loader stateToViz updateViz renderViz keyboardHandler respondToStateChange onExit = do
  initHistory <- loadOrDefault loader defaultState filenameMaybe
  let s = cur initHistory
      initViz = updateViz 0 s initViz'
      initWorld = GuiState initHistory 0 (stateToViz (cur initHistory) initViz s 0) initHistory
      worldToPicture (GuiState h t v _) = renderViz t (cur h) v
      eventHandler ev@(EventKey key Down modifiers _) gs@(GuiState h t v lastH) = do
        --msp ("EV", ev)
        command <- keyboardHandler (cur h) (key, modifiers)
        --msp ("COMMAND", command)
        h' <- execute command h lastH saver loader onExit
        --msp ("boing", length (currentGroup (cur h)), length (currentGroup (cur h')))
        -- If we just saved, then replace lastSave with current history
        let newLastSave = case command of Save _ -> h'
                                          _ -> lastH
        --msp ("hmmm", hWhere h', hWhere lastH, hWhere newLastSave)
        if h == h' && (cur h) == (cur h')
           then return gs { lastSave = newLastSave }
           else do respondToStateChange (cur h) (cur h')
                   return $ GuiState h' t (stateToViz (cur h) v (cur h') t) newLastSave
      eventHandler e gs = do
        --msp $ "?? " ++ (show e)
        return gs
      stepIteration dt (GuiState h t v lastH) = return $ GuiState h (t + dt) (updateViz dt (cur h) v) lastH
   in playIO displayMode bgColor 20 initWorld worldToPicture eventHandler stepIteration
  where displayMode = InWindow "Rhythmr" (windowWidth, windowHeight) (440, 125)
        bgColor = white

loadOrDefault :: (Binary t, Read t) => Loader (History s) t -> s -> Maybe FilePath -> IO (History s)
loadOrDefault loader s (Just filename) = do
  b <- doesFileExist filename
  --msp ("gosh", filename, b)
  if b then load filename loader else return $ start s
loadOrDefault _ s Nothing = return $ start s

-- Loading a history with 169 states -- 1.5M on disk -- pins all four cpus at
-- 100%. I don't know why. Further investigation was even more baffling.
--
-- It sort of seems like
--   (1) that's too much state for a program with a 93M heap
--   (2) the heap is not growing
--   (3) it's GC'ing like mad
loadHistoryAndSurviveSomehow :: (Show s, Read t, Show t, Binary t) => FilePath -> Loader (History s) t -> IO (History s)
loadHistoryAndSurviveSomehow filename loader = do
  -- Normal: pins
  -- load filename loader

  h <- load filename loader

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

displayUndoState :: History s -> IO (History s)
displayUndoState h = do
  putStrLn $ show $ hWhere h
  return h

execute :: (Eq s, Show s, Read t, Show t, Binary t) => GuiCommand s -> History s -> History s -> Saver (History s) t -> Loader (History s) t -> IO () -> IO (History s)
execute command h lastH saver loader onExit =
  case command of NewState s -> return $ update h s
                  Save filename -> do save filename saver h
                                      return h
                  SaveAndQuit filename -> do save filename saver h
                                             onExit
                                             exitSuccess
                  -- Load filename -> load filename loader
                  Load filename -> loadHistoryAndSurviveSomehow filename loader
                  -- Load filename -> do h <- load filename loader
                  --                     msp ("len", length (toList h))
                  --                     let h' = fromList (take 1 (toList h))
                  --                     return h'
                  Undo -> displayUndoState $ undo h
                  Redo -> displayUndoState $ redo h
                  UndoFully -> return $ toBeginning h
                  RedoFully -> return $ toEnd h
                  QuitWithoutSaving -> do onExit
                                          exitSuccess
                  Quit -> do  msp ("clean?", hWhere h, hWhere lastH)
                              if h == lastH then do onExit
                                                    exitSuccess
                                            else do putStrLn "Save first!"
                                                    return h
                  GuiCommands (c:cs) -> do
                    h' <- execute c h lastH saver loader onExit
                    execute (GuiCommands cs) h lastH saver loader onExit
                  GuiCommands [] -> return h
                  DoNothing -> return h
