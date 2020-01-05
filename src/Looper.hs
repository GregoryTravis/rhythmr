module Looper
( startLooper
, setLoop
) where

import qualified Data.StorableVector as SV
import Data.StorableVector.Base as SVB
import Foreign.ForeignPtr
import Foreign.Ptr

import Sound

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

startLooper = do
  sound <- readSound "aloop.wav"
  let Sound { samples = buffer } = sound
  init_audio
  writeAudioAllAtOnce buffer
  writeAudioAllAtOnce buffer
  writeAudioAllAtOnce buffer
  writeAudioAllAtOnce buffer
  term_audio

setLoop = undefined

writeAudioAllAtOnce :: Vector Float -> IO ()
writeAudioAllAtOnce v =
  let (fp, 0, length) = SVB.toForeignPtr v
   in withForeignPtr fp (\ptr -> write_audio ptr (length `div` 2))
