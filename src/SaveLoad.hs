--{-# LANGUAGE BlockArguments #-}

module SaveLoad
( Loader
, Saver
, load
, save ) where

-- import Control.Exception (finally)
-- import System.Console.ANSI
-- import System.Posix.IO (stdInput)
-- import System.Posix.Terminal

import History
import Util

-- s is state, t is the storable representation
-- Loader result is in IO since you might have to load stuff
-- Loader takes the current state in case it has a unique resource you need to re-use
type Saver s t = s -> t
type Loader s t = s -> t -> IO s

load :: Read t => s -> String -> Loader s t -> IO (History s)
load currentState filename loader = do
  fileContentsString <- readFile filename
  runEm $ loader currentState <$> read fileContentsString

save :: Show t => String -> Saver s t -> History s -> IO ()
save filename saver history = do
  let fileContentsString = show (saver <$> history)
  writeFile filename fileContentsString
