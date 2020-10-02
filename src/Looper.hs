module Looper
( Looper
, withPortaudio
, withLooper
, setZound
, setZoundFromTheTop
, noSound
, getProgress
, setVolume
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (finally)
import Data.IORef
import qualified Data.StorableVector as SV
import Data.StorableVector.Base as SVB
import Foreign.ForeignPtr
import Foreign.Ptr

import Zounds
import Util

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

-- This used to work
-- granularity = 64
granularity = 64 * 64

initVolume = 1 -- demo mode: 4.9382716e-2

-- Looper sv iv lv vv restartV
--   sv: the samples
--   iv: played so far; only used by getProgress
--   lv: the length of the current sound
--   vv: volume
data Looper = Looper (MVar FSamples) (IORef Int) (IORef Int) (IORef Float) (IORef Bool)

withPortaudio :: IO a -> IO a
withPortaudio action = do
  init_audio
  action `finally` term_audio

withLooper :: (Looper -> IO a) -> IO a
withLooper action = do
  sv <- newEmptyMVar
  iv <- newIORef 0
  lv <- newIORef 1
  vv <- newIORef initVolume
  restartV <- newIORef False
  let looper = Looper sv iv lv vv restartV
  threadId <- forkIO $ loop looper
  let cleanup = killThread threadId
  (action looper) `finally` cleanup

setZound :: Looper -> Zound -> IO ()
setZound l sound = do
  --msp ("set", SV.length (amples sound))
  -- Dum way to make sure the sound is evaluated before putting it in the mvar
  let (Looper sv _ lv vv _) = (samples sound) `seq` l
      samplesF = samplesAsFloats sound
  empty <- isEmptyMVar sv
  if empty
     then putMVar sv samplesF
     else do swapMVar sv samplesF
             return ()
  writeIORef lv (numFrames sound * 2)

setZoundFromTheTop :: Looper -> Zound -> IO ()
setZoundFromTheTop l@(Looper _ _ _ _ restartV) z = do
  writeIORef restartV True
  setZound l z

noSound :: Looper -> IO ()
noSound (Looper sv iv _ _ _) = do
  msp "clearing"
  empty <- isEmptyMVar sv
  if empty
     then return ()
     else do takeMVar sv
             return ()

loop :: Looper -> IO ()
loop looper = loop' looper 0
loop' :: Looper -> Int -> IO ()
loop' l@(Looper sv iv _ vv restartV) currentIndex' = do
  --msp currentIndex
  buffer <- readMVar sv
  volume <- readIORef vv
  restart <- readIORef restartV
  if restart then do writeIORef restartV False 
                     msp "restart"
             else return ()
  let currentIndex = if restart then 0 else currentIndex'
  let grain = SV.take granularity (SV.drop currentIndex buffer)
      grainLength = SV.length grain
      nextCurrentIndex = (currentIndex + (grainLength * 1)) `mod` (SV.length buffer)
  -- if nextCurrentIndex < currentIndex
  --    then msp "loop restarting"
  --    else return ()
  writeAudioAllAtOnce volume grain
  writeIORef iv currentIndex
  --msp ("wrote", currentIndex)
  loop' l nextCurrentIndex

-- returns 0..1
getProgress :: Looper -> IO Float
getProgress (Looper _ iv lv vv _) = do
  currentIndex <- readIORef iv
  length <- readIORef lv
  return $ fromIntegral currentIndex / fromIntegral length

  -- soundMaybe <- tryTakeMVar sv
  -- let prog (Zound { samples = buffer }) =
  --       let numGrains = SV.length buffer
  --        in fromIntegral currentIndex / fromIntegral numGrains
  -- return $ case soundMaybe of Just sound -> Just $ prog sound
  --                             Nothing -> Nothing
  --return 0.4
  ------msp ("prog", currentIndex)
  ----let numGrains = SV.length buffer
  ----return $ fromIntegral currentIndex / fromIntegral numGrains

writeAudioAllAtOnce :: Float -> Vector Float -> IO ()
writeAudioAllAtOnce volume v' = do
  --msp ("scale", volume)
  let v = SV.map (* volume) v' :: Vector Float
  let (fpAll, startFloats, length) = SVB.toForeignPtr v
      startBytes = startFloats * 4
      fp = fpAll `plusForeignPtr` startBytes
  --msp ("oh", startBytes, length, fp)
  withForeignPtr fp (\ptr -> write_audio ptr (length `div` 2))

setVolume :: Float -> Looper -> IO ()
setVolume volume (Looper _ _ _ vv _)
  | volume >= 0.0 && volume <= 1.0 = writeIORef vv (eeesp ("volume", volume) (volume * volume))
  | otherwise = return ()
