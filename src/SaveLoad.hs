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

import qualified History as H
import Util

-- s is state, t is the storable representation
-- Loader result is in IO since you might have to load stuff
-- Loader takes the current state in case it has a unique resource you need to re-use
type Saver s t = s -> t
type Loader s t = s -> t -> s

load :: Read t => s -> String -> Loader s t -> IO (H.History s)
load currentState filename loader = do
  fileContentsString <- readFile filename
  return $ H.map (loader currentState) (read fileContentsString)
  -- let repZipper :: Z.Zipper t
  --     repZipper = read fileContentsString
  --     stateZipper :: Z.Zipper s
  --     stateZipper = Z.zmap (loader currentState) repZipper
  -- return $ H.History stateZipper

save :: Show t => String -> Saver s t -> H.History s -> IO ()
save filename saver history = do
  let fileContentsString = show (H.map saver history)
  writeFile filename fileContentsString
