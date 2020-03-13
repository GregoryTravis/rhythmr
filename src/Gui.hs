{-# LANGUAGE NamedFieldPuns #-}

module Gui
  ( guiMain
  , GuiCommand(..)
  , windowWidth
  , windowHeight ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
--import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (finally)
import Control.Monad.STM (atomically)
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

data GuiState s v = GuiState (History s) v

-- lol "Save String"
data GuiCommand s = NewState s | Save String | Load String | Undo | Redo | Quit | DoNothing
  deriving Show

guiMain :: (Eq s, Show s, Read t, Show t) => s -> Saver s t -> Loader s t -> (s -> s -> v) -> (v -> Picture) -> (Float -> v -> v) -> (s -> Char -> IO (GuiCommand s)) -> (s -> s -> IO ()) -> IO ()
guiMain s saver loader statesToViz renderViz advanceViz keyboardHandler respondToStateChange =
  let initWorld = GuiState (start s) (statesToViz s s)
      worldToPicture (GuiState _ v) = return $ renderViz v
      eventHandler (EventKey (SpecialKey KeyEsc) Down x y) gs = eventHandler (EventKey (Char '\ESC') Down x y) gs
      eventHandler (EventKey (Char c) Down _ _) gs@(GuiState h v) = do
        command <- keyboardHandler (cur h) c
        --msp command
        h' <- execute command h saver loader
        if h == h'
           then return gs
           else do respondToStateChange (cur h) (cur h')
                   return $ GuiState h' (statesToViz (cur h) (cur h'))
      eventHandler e gs = return gs
      stepIteration dt (GuiState h v) = return $ GuiState h (advanceViz dt v)
   in playIO displayMode bgColor 100 initWorld worldToPicture eventHandler stepIteration
  where displayMode = InWindow "Nice Window" (windowWidth, windowHeight) (810, 10)
        bgColor = white

execute :: (Read t, Show t) => GuiCommand s -> History s -> Saver s t -> Loader s t -> IO (History s)
execute command h saver loader =
  case command of NewState s -> return $ update h s
                  Save filename -> do save filename saver h
                                      return h
                  Load filename -> load (cur h) filename loader
                  Undo -> return $ undo h
                  Redo -> return $ redo h
                  Quit -> exitSuccess
