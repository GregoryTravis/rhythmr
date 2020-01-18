module Looper
( Nooper
, withNooper
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

data Nooper = Nooper (MVar Sound)

withNooper :: (Nooper -> IO a) -> IO a
withNooper action = do
  sv <- newEmptyMVar
  let looper = Nooper sv
  threadId <- forkIO $ noop looper
  let cleanup = killThread threadId
  (action looper) `finally` cleanup

setSound :: Nooper -> Sound -> IO ()
setSound (Nooper sv) sound = do
  msp ("set", SV.length (samples sound))
  empty <- isEmptyMVar sv
  if empty
     then putMVar sv sound
     else do swapMVar sv sound
             return ()

noop :: Nooper -> IO ()
noop looper = noop' looper 0
noop' :: Nooper -> Int -> IO ()
noop' l@(Nooper sv) currentIndex = do
  --msp currentIndex
  Sound { samples = buffer } <- readMVar sv
  let grain = SV.take granularity (SV.drop currentIndex buffer)
      grainLength = SV.length grain
      nextCurrentIndex = (currentIndex + (grainLength * 1)) `mod` (SV.length buffer)
  writeAudioAllAtOnce grain
  noop' l nextCurrentIndex

writeAudioAllAtOnce :: Vector Float -> IO ()
writeAudioAllAtOnce v = do
  let (fpAll, startFloats, length) = SVB.toForeignPtr v
      startBytes = startFloats * 4
      fp = fpAll `plusForeignPtr` startBytes
  --msp ("oh", startBytes, length, fp)
  withForeignPtr fp (\ptr -> write_audio ptr (length `div` 2))
