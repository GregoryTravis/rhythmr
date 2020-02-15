module Gui
( gfxMain ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime, SystemTime)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.Random

import Util

-- x position of circle center
data World = World

gfxMain = do
  playIO displayMode bgColor 120 World worldToPicture eventHandler stepIteration
  where displayMode = InWindow "Nice Window" (800, 800) (810, 10)
        bgColor = white

worldToPicture :: World -> IO Picture
worldToPicture _ = do
  return $ lineLoop $ rectanglePath 30 50

--"Event EventKey (SpecialKey KeyEsc) Down (Modifiers {shift = Up, ctrl = Up, alt = Up}) (383.0,20.0)"
eventHandler :: Event -> World -> IO World
eventHandler e w = do
  msp $ "Event " ++ (show e)
  quitOnEsc e
  return w
  where
    quitOnEsc (EventKey (SpecialKey KeyEsc) Down _ _) = exitSuccess
    quitOnEsc _                                       = return ()

stepIteration :: Float -> World -> IO World
stepIteration dt w = return w
