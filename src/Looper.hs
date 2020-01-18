module Looper
( Looper
, initAudio
, withLooper
, setSound
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

import Sound
import Util

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

granularity = 64

data Looper = Looper (MVar Sound)

initAudio = init_audio

withLooper :: (Looper -> IO a) -> IO a
withLooper action = do
  sv <- newEmptyMVar
  let looper = Looper sv
  threadId <- forkIO $ loop looper
  let cleanup = killThread threadId
  (action looper) `finally` cleanup

setSound :: Looper -> Sound -> IO ()
setSound (Looper sv) sound = do
  msp ("set", SV.length (samples sound))
  empty <- isEmptyMVar sv
  if empty
     then putMVar sv sound
     else do swapMVar sv sound
             return ()

loop :: Looper -> IO ()
loop looper = loop' looper 0
loop' :: Looper -> Int -> IO ()
loop' l@(Looper sv) currentIndex = do
  --msp currentIndex
  Sound { samples = buffer } <- readMVar sv
  let grain = SV.take granularity (SV.drop currentIndex buffer)
      grainLength = SV.length grain
      nextCurrentIndex = (currentIndex + (grainLength * 1)) `mod` (SV.length buffer)
  writeAudioAllAtOnce grain
  loop' l nextCurrentIndex

writeAudioAllAtOnce :: Vector Float -> IO ()
writeAudioAllAtOnce v = do
  let (fpAll, startFloats, length) = SVB.toForeignPtr v
      startBytes = startFloats * 4
      fp = fpAll `plusForeignPtr` startBytes
  --msp ("oh", startBytes, length, fp)
  withForeignPtr fp (\ptr -> write_audio ptr (length `div` 2))
