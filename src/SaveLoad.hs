--{-# LANGUAGE BlockArguments #-}

module TUI
( Loader
, Saver ) where

-- import Control.Exception (finally)
-- import System.Console.ANSI
-- import System.Posix.IO (stdInput)
-- import System.Posix.Terminal

import Util
import qualified Zipper as Z

-- s is state, t is the storable representation
-- Loader result is in IO since you might have to load stuff
-- Loader takes the current state in case it has a unique resource you need to re-use
type Saver s t = [s] -> t
type Loader s t = s -> t -> IO [s]

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
