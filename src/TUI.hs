{-# LANGUAGE BlockArguments #-}

module TUI
( KeyboardHandler
, KeyboardHandlerWrapper
, Displayer
, editor
, runEditor ) where

import Control.Exception (finally)
import System.Console.ANSI
import System.Posix.IO (stdInput)
import System.Posix.Terminal

import Util

---- Editor
--clearScreen = putStr "\027[2J"
setCursorPos x y = setCursorPosition y x
resetTerm = do
  setCursorPos 0 0
  clearScreen
  --putStr "\ESC[7m"

-- Bool is exit?
type KeyboardHandler s = s -> Char -> IO (s, Bool)
type Displayer s = s -> String
type KeyboardHandlerWrapper s = KeyboardHandler s -> KeyboardHandler s

editor :: s -> KeyboardHandler s -> Displayer s -> IO ()
editor initState keyboardHandler displayer = do
  -- TODO don't need two resetTerms?
  resetTerm
  let loop s = do
        putStrLn $ displayer s
        c <- getChar
        --msp $ "char " ++ (show c)
        (s', exitP) <- keyboardHandler s c
        resetTerm
        if exitP then return () else loop s'
   in loop initState
  msp "editor done"

----

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

runEditor editor = withRawInput 0 1 $ do
  -- let loop = do
  --       c <- getChar
  --       msp $ "char " ++ (show c)
  --       if c /= '\ESC' then loop else return ()
  --  in loop
  --editor (State []) keyboardHandler displayer
  editor
  msp "tui"
