--{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SaveLoad
( Loader
, Saver
, saveB
, loadB
, load
, save ) where

import Data.Binary
-- import Control.Exception (finally)
-- import System.Console.ANSI
-- import System.Posix.IO (stdInput)
-- import System.Posix.Terminal

import History
import Util

binary = False
load :: (Binary t, Read t) => String -> Loader s t -> IO (History s)
load = if binary then loadB else loadT
save :: (Binary t, Show t) => String -> Saver s t -> History s -> IO ()
save = if binary then saveB else saveT

-- s is state, t is the storable representation
-- Loader result is in IO since you might have to load stuff
-- Loader takes the current state in case it has a unique resource you need to re-use
type Saver s t = History s -> History t
type Loader s t = History t -> History s

loadT :: forall s t. (Binary t, Read t) => FilePath -> Loader s t -> IO (History s)
loadT filename loader = do
  s <- readFile filename
  let r :: Read t => History t
      r = read s
      h :: History s
      h = loader r
  msp $ ("load history size (text)", length $ toList h)
  return h

loadB :: (Binary t, Read t) => String -> Loader s t -> IO (History s)
loadB filename loader = do
  r <- decodeFile filename
  let h = loader r
  msp $ ("load history size (binary)", length $ toList h)
  return h

saveT :: (Binary t, Show t) => String -> Saver s t -> History s -> IO ()
saveT filename saver history = do
  msp $ ("save history size (text)", length $ toList history)
  let s = show (saver history)
  writeFile filename s

saveB :: (Binary t, Show t) => String -> Saver s t -> History s -> IO ()
saveB filename saver history = do
  msp $ ("save history size (binary)", length $ toList history)
  let r = saver history
  encodeFile filename r
