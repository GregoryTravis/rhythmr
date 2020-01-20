{-# LANGUAGE BlockArguments #-}

module TUI
( KeyboardHandler
, KeyboardHandlerWrapper
, KHResult(..)
, Displayer
, StateChangeHandler
, editor
, runEditor ) where

import Control.Exception (finally)
import System.Console.ANSI
import System.Posix.IO (stdInput)
import System.Posix.Terminal

import Util
import qualified Zipper as Z

data KHResult s = SetState s | Quit | Undo | Redo | DoNothing deriving (Eq, Show)

setCursorPos x y = setCursorPosition y x
resetTerm = do
  setCursorPos 0 0
  clearScreen

-- Bool is exit?
type KeyboardHandler s = s -> Char -> IO (KHResult s)
type Displayer s = s -> String
type StateChangeHandler s = s -> s -> IO ()
type KeyboardHandlerWrapper s = KeyboardHandler s -> KeyboardHandler s

editor :: Eq s => s -> KeyboardHandler s -> Displayer s -> StateChangeHandler s -> IO ()
editor initState keyboardHandler displayer stateChangeHandler = do
  let loop history = do
        resetTerm
        -- msp $ "History: " ++ show (Z.zwhere history)
        let s = Z.cur history
        putStrLn $ displayer s
        c <- getChar
        --msp $ "char " ++ (show c)
        command <- keyboardHandler s c
        let history' = case command of SetState s -> Z.push (Z.removeTop history) s
                                       Undo -> Z.downMaybe history
                                       Redo -> Z.upMaybe history
                                       TUI.Quit -> history
        stateChangeHandler (Z.cur history) (Z.cur history')
        if command == TUI.Quit
           then return ()
           else loop history'
   in loop (Z.makeZipper initState)
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
