module Display
( DisplayRequest(..)
, DispayResponse(..)
, displayServer
, withDisplay
, displayMain
, displaySend ) where

import Control.Concurrent (forkIO, threadDelay)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import RPC
import Util

data Disp = Disp RPCClient

data DisplayRequest = Circ Float deriving (Read, Show)
data DispayResponse = DispayResponse deriving (Read, Show)

displayServer = do
  forkIO $ do
    rpcServer 3000 serverHandler
    threadDelay $ 7200 * 1000000
  return ()

serverHandler :: DisplayRequest -> IO DispayResponse

serverHandler (Circ w) = do
  -- display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle w)
  displayMain
  return DispayResponse

-- x position of circle center
type World = Int
initialWorld = 0

--displayMain = animate (InWindow "Nice Window" (200, 200) (10, 10)) white anim
displayMain = playIO displayMode bgColor 2 initialWorld worldToPicture eventHandler stepIteration
  where displayMode = InWindow "Nice Window" (200, 200) (10, 10)
        bgColor = white

worldToPicture :: World -> IO Picture
worldToPicture x = return $ translate (fromIntegral x) 0 $ Circle $ 20 -- + t * 20

eventHandler :: Event -> World -> IO World
eventHandler e w = do
  msp $ "Event " ++ (show e)
  return w

stepIteration :: Float -> World -> IO World
stepIteration dt w = do
  msp $ "Step " ++ (show dt)
  return w

anim :: Float -> Picture
anim t = Circle $ 20 + t * 20

withDisplay :: (Disp -> IO a) -> IO a
withDisplay handler = withRPCClient "127.0.0.1" 3000 $ \c -> handler $ Disp c

displaySend :: Disp -> DisplayRequest -> IO DispayResponse
displaySend (Disp c) request = rpcSend c request
