module Looper
( Looper
, LoopCommand(..)
, withLooper
, sendCommand
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Exception (finally)
import Data.IORef
import qualified Data.StorableVector as SV
import Data.StorableVector.Base as SVB
import Foreign.ForeignPtr
import Foreign.Ptr

import Sound
import Util

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

-- TODO: StopAndWait?
data LoopCommand = Play Sound | Stop deriving Show
-- TODO probably doesn't need to be a Maybe, or even part of this?
data Looper = Looper (Chan LoopCommand) (IORef (Maybe Sound)) -- (MV.MVar Sound)

withLooper :: (Looper -> IO a) -> IO a
withLooper f = do
  looper <- startLooper
  let cleanup = sendCommand looper Stop
  (f looper) `finally` cleanup

startLooper = do
  init_audio
  chan <- newChan
  --cur <- MV.newEmptyMVar
  cur <- newIORef Nothing
  let looper = Looper chan cur
  forkIO (backgroundLooper looper)
  return looper

backgroundLooper :: Looper -> IO ()
backgroundLooper looper = do
  waitForFirstSound looper
  forkIO $ updateIORef looper
  playCurrentSound4EVA looper
  msp "backgroundLooper done"

waitForFirstSound :: Looper -> IO ()
waitForFirstSound (Looper chan ioref) = do
  command <- readChan chan
  --msp $ "Got " ++ (show command)
  case command of Play sound -> do writeIORef ioref (Just sound)
                                   return ()

updateIORef :: Looper -> IO ()
updateIORef looper@(Looper chan ioref) = do
  command <- readChan chan
  --msp $ "Got " ++ (show command)
  handle command
  where handle :: LoopCommand -> IO ()
        handle (Play sound) = do
          --msp $ "Setting " ++ (show sound)
          writeIORef ioref (Just sound)
          updateIORef looper
        handle Stop = do
          msp $ "Stopping"
          writeIORef ioref Nothing
          msp "updateIORef done"

-- Assumes ioref is not Nothing
playCurrentSound4EVA :: Looper -> IO ()
playCurrentSound4EVA looper@(Looper chan ioref) = do
  soundMaybe <- readIORef ioref
  playOrQuit soundMaybe
  where playOrQuit :: Maybe Sound -> IO ()
        playOrQuit (Just sound) = do
          --msp $ "Playing " ++ (show sound)
          let Sound { samples = buffer } = sound
          writeAudioAllAtOnce buffer
          playCurrentSound4EVA looper
        playOrQuit Nothing = return ()

sendCommand :: Looper -> LoopCommand -> IO ()
sendCommand (Looper chan _) command = do
  writeChan chan command

writeAudioAllAtOnce :: Vector Float -> IO ()
writeAudioAllAtOnce v =
  let (fp, 0, length) = SVB.toForeignPtr v
   in withForeignPtr fp (\ptr -> write_audio ptr (length `div` 2))
