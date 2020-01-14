{-# LANGUAGE BlockArguments #-}
module TUI (tui) where

import Control.Exception (finally)
import System.Posix.IO (stdInput)
import System.Posix.Terminal

import Util

-- Taken from https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input/36297897#36297897
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime action = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings = 
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- when we're done -}
  let revert = do setTerminalAttributes stdInput oldTermSettings Immediately
                  return ()

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  action `finally` revert

tui = withRawInput 0 1 $ do
  let loop = do
        c <- getChar
        msp $ "char " ++ (show c)
        if c /= '\ESC' then loop else return ()
   in loop
  msp "tui"
