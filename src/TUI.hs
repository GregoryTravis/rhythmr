{-# LANGUAGE BlockArguments #-}

module TUI
( KeyboardHandler
, KeyboardHandlerWrapper
, KHResult(..)
, Displayer
, StateChangeHandler
, Loader
, Saver
, editor
, runEditor ) where

import Control.Exception (finally)
import System.Console.ANSI
import System.Posix.IO (stdInput)
import System.Posix.Terminal

import Util
import qualified Zipper as Z

-- "Save String" LOL
data KHResult s = SetState s | Quit | Undo | Redo | DoNothing | Load String | Save String
  deriving (Eq, Show)

setCursorPos x y = setCursorPosition y x
resetTerm = do
  setCursorPos 0 0
  clearScreen

-- Bool is exit?
type KeyboardHandler s = s -> Char -> IO (KHResult s)
type Displayer s = s -> String
type StateChangeHandler s = s -> s -> IO ()
type KeyboardHandlerWrapper s = KeyboardHandler s -> KeyboardHandler s

-- s is state, t is the storable representation
-- Loader result is in IO since you might have to load stuff
-- Loader takes the current state in case it has a unique resource you need to re-use
type Saver s t = [s] -> t
type Loader s t = s -> t -> IO [s]

editor :: (Eq s, Read t, Show t) =>
            s -> KeyboardHandler s -> Displayer s -> StateChangeHandler s ->
                 Loader s t -> Saver s t -> IO ()
editor initState keyboardHandler displayer stateChangeHandler loader saver = do
  let loop history = do
        --resetTerm
        -- msp $ "History: " ++ show (Z.zwhere history)
        let s = Z.cur history
        putStrLn $ displayer s
        c <- getChar
        --msp $ "char " ++ (show c)
        command <- keyboardHandler s c
        history' <- case command of SetState s -> return $ Z.push (Z.removeTop history) s
                                    Undo -> return $ Z.downMaybe history
                                    Redo -> return $ Z.upMaybe history
                                    Load filename -> load (Z.cur history) filename loader
                                    Save filename -> do save filename saver history
                                                        return history
                                    TUI.Quit -> return history
        stateChangeHandler (Z.cur history) (Z.cur history')
        if command == TUI.Quit
           then return ()
           else loop history'
   in loop (Z.makeZipper initState)
  msp "editor done"

load :: Read t => s -> String -> Loader s t -> IO (Z.Zipper s)
load currentState filename loader = do
  fileContentsString <- readFile filename
  let rep = read fileContentsString
  states <- loader currentState rep
  return $ Z.fromList states

save :: Show t => String -> Saver s t -> Z.Zipper s -> IO ()
save filename saver history = do
  let states = Z.toList history
      rep = saver states
      fileContentsString = show rep
  writeFile filename fileContentsString

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
