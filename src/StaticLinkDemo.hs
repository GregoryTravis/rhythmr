module Main where

import Foreign.ForeignPtr
import Foreign.Ptr

foreign import ccall "init_audio" init_audio :: IO ()
foreign import ccall "write_audio" write_audio :: Ptr Float -> Int -> IO ()
foreign import ccall "term_audio" term_audio :: IO ()

main = do
  putStrLn "hey"
  init_audio
  putStrLn "inside audio"
  term_audio
