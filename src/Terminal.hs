module Terminal
( TerminalRequest(..)
, TerminalResponse(..)
, displayServer
, withTerminal
, displayMain
, displaySend ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime, SystemTime)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import RPC
import Util

data Terminal = Terminal RPCClient

data TerminalRequest = Circ Float deriving (Read, Show)
data TerminalResponse = TerminalResponse deriving (Read, Show)

displayServer = do
  forkIO $ do
    rpcServer 3000 serverHandler
  threadDelay $ 7200 * 1000000
  return ()

serverHandler :: TerminalRequest -> IO TerminalResponse

serverHandler (Circ w) = do
  -- display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle w)
  displayMain
  return TerminalResponse

ww = 800
hh = ww

-- x position of circle center
data World = World Float (Maybe SystemTime) [(Float, Float, Float)]

-- Doesn't exist in this version?
nominalDiffTimeToSeconds :: NominalDiffTime -> Float
--nominalDiffTimeToSeconds ndt = eesp (show (ndt, (length (reverse (show ndt))))) 3.4
--nominalDiffTimeToSeconds = read . reverse . esp . tail . reverse . show . show
nominalDiffTimeToSeconds s =
  let ('s' : s') = reverse (show s)
      (s'') = reverse s'
   in read s''


fpsBufferSize = 30
fps :: (x -> y -> IO a) -> IO (x -> y -> IO a)
fps action = do
  now <- getSystemTime
  mvar <- newMVar (take fpsBufferSize $ repeat now)
  let wrapped x y = do
        buffer <- takeMVar mvar
        now <- getSystemTime
        let diff :: Float
            diff = nominalDiffTimeToSeconds $ (systemToUTCTime now) `diffUTCTime` (systemToUTCTime (head buffer))
            avgDiff = diff / (fromIntegral fpsBufferSize)
            fps = 1.0 / avgDiff
            newBuffer = (tail buffer) ++ [now]
        putMVar mvar newBuffer
        msp $ "FPS " ++ show fps
        action x y
  return wrapped

--displayMain = animate (InWindow "Nice Window" (200, 200) (10, 10)) white anim
displayMain = do
  let num = 400
      mx = fromIntegral $ (max ww hh) `div` 2
      range = ((-mx), mx)
      xs = take num $ randomRs range (mkStdGen 37)
      ys = take num $ randomRs range (mkStdGen 77737)
      as = take num $ randomRs ((-2.0), 2.0) (mkStdGen 2000)
      initialWorld = World 0 Nothing (zip3 xs ys as)
  stepIteration' <- fps stepIteration
  playIO displayMode bgColor 120 initialWorld worldToPicture eventHandler stepIteration'
  where displayMode = InWindow "Nice Window" (ww, hh) (10, 10)
        bgColor = white

worldToPicture :: World -> IO Picture
worldToPicture (World t st pts) = do
  let aBox = lineLoop $ rectanglePath 30 50
      sn = sin (t * 10)
      --box :: Float -> (Float, Float) -> Picture
      box t (x, y, a) = translate (x + (sn*a*8.0)) y aBox
      pix = Pictures $ map (box t) pts
  --msp pix
  return pix
--worldToPicture t = return $ rotate (t*3.14)

eventHandler :: Event -> World -> IO World
eventHandler e w = do
  msp $ "Event " ++ (show e)
  return w

stepIteration :: Float -> World -> IO World
stepIteration dt (World t st pts) = do
  --msp $ "Step " ++ (show dt)
  newST <- getSystemTime
  --msp pts
  case st of Just st -> do
                          let diff = (systemToUTCTime newST) `diffUTCTime` (systemToUTCTime st)
                          --msp diff
                          return ()
             Nothing -> return ()
  return $ World (t+dt) (Just newST) pts

anim :: Float -> Picture
anim t = Circle $ 20 + t * 20

withTerminal :: (Terminal -> IO a) -> IO a
withTerminal handler = withRPCClient "127.0.0.1" 3000 $ \c -> handler $ Terminal c

displaySend :: Terminal -> TerminalRequest -> IO TerminalResponse
displaySend (Terminal c) request = rpcSend c request
