{-# LANGUAGE BlockArguments #-}

module TUI
( --KeyboardHandler
--, KeyboardHandlerWrapper
--, KHResult(..)
 --Displayer
--, StateChangeHandler
 ) where

import Control.Exception (finally)
import System.Console.ANSI
import System.Posix.IO (stdInput)
import System.Posix.Terminal

import Util
import qualified Zipper as Z

-- "Save String" LOL
-- data KHResult s = SetState s | Quit | Undo | Redo | DoNothing | Load String | Save String
--   deriving (Eq, Show)




--editor :: (Eq s, Read t, Show t) =>
--            s -> KeyboardHandler s -> Displayer s -> StateChangeHandler s ->
--                 Loader s t -> Saver s t -> IO ()
--editor initState keyboardHandler displayer stateChangeHandler loader saver = do
--  let loop history = do
--        --resetTerm
--        -- msp $ "History: " ++ show (Z.zwhere history)
--        let s = Z.cur history
--        putStrLn $ displayer s
--        c <- getChar
--        --msp $ "char " ++ (show c)
--        command <- keyboardHandler s c
--        history' <- case command of SetState s -> return $ Z.push (Z.removeTop history) s
--                                    Undo -> return $ Z.downMaybe history
--                                    Redo -> return $ Z.upMaybe history
--                                    Load filename -> load (Z.cur history) filename loader
--                                    Save filename -> do save filename saver history
--                                                        return history
--                                    TUI.Quit -> return history
--        stateChangeHandler (Z.cur history) (Z.cur history')
--        if command == TUI.Quit
--           then return ()
--           else loop history'
--   in loop (Z.makeZipper initState)
--  msp "editor done"


----

