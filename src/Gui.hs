{-# LANGUAGE NamedFieldPuns #-}

module Gui
( withGui
, Gfx(..)
, Node(..) ) where

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

import Util

data Node = Node { pos :: V2 Float
                 , dest :: V2 Float }
  deriving Show

data Gfx = Gfx { nodes :: [Node] }
  deriving Show

velocity = 40.0

updateNode :: Float -> Float -> Node -> Node
updateNode t dt n@(Node { pos, dest }) = n { pos = newPos }
  where newPos = pos + (velocity * dt *^ signorm (dest - pos))

stepIteration :: Float -> Float -> Gfx -> IO Gfx
stepIteration t dt (Gfx { nodes }) = return $ Gfx (map (updateNode t dt) nodes)

data Cumulator a = Cumulator (Float, a)
cumulative :: (Float -> Float -> a -> IO a) -> (Float -> Cumulator a -> IO (Cumulator a))
cumulative si = newSi
  where newSi dt (Cumulator (t, w)) = do
          let t' = t + dt
          newW <- si t' dt w
          return $ Cumulator (t', newW)

cumulativePlayIO dm c rate w wtp eh si =  playIO dm c rate (Cumulator (0.0, w)) wtp' eh' si'
  where si' = cumulative si
        wtp' (Cumulator (_, w)) = wtp w
        eh' e (Cumulator (t, w)) = do
          w' <- eh e w
          return $ Cumulator (t, w')

gfxMain :: Gfx -> TChan Gfx -> IO ()
gfxMain gfx gfxChan = do
  cumulativePlayIO displayMode bgColor 10 gfx worldToPicture eventHandler stepIteration
  where displayMode = InWindow "Nice Window" (800, 800) (810, 10)
        bgColor = white
        --stepIteration' f = stepIteration (float2Float f)

worldToPicture :: Gfx -> IO Picture
worldToPicture (Gfx nodes) = return $ Pictures (map renderNode nodes)

renderNode :: Node -> Picture
renderNode (Node { pos = V2 x y }) = translate x y $ lineLoop (rectanglePath 30 50)

--"Event EventKey (SpecialKey KeyEsc) Down (Modifiers {shift = Up, ctrl = Up, alt = Up}) (383.0,20.0)"
eventHandler :: Event -> Gfx -> IO Gfx
eventHandler e w = do
  msp $ "Event " ++ (show e)
  quitOnEsc e
  return w
  where
    quitOnEsc (EventKey (SpecialKey KeyEsc) Down _ _) = exitSuccess
    quitOnEsc _                                       = return ()

withGui :: Gfx -> (TChan Gfx -> IO ()) -> IO ()
withGui gfx callback = do
  gfxChan <- atomically newTChan
  let action = callback gfxChan
  threadId <- forkIO (gfxMain gfx gfxChan)
  action `finally` (killThread threadId)
