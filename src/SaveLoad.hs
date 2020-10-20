--{-# LANGUAGE BlockArguments #-}

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
type Saver s t = s -> t
type Loader s t = t -> IO s

loadT :: (Binary t, Read t) => String -> Loader s t -> IO (History s)
loadT filename loader = do
  fileContentsString <- readFile filename
  h <- runEm $ loader <$> read fileContentsString
  msp $ ("load history size (text", length $ toList h)
  return h

loadB :: (Binary t, Read t) => String -> Loader s t -> IO (History s)
loadB filename loader = do
  reps <- decodeFile filename
  h <- runEm $ loader <$> reps
  msp $ ("load history size (text)", length $ toList h)
  return h

saveT :: (Binary t, Show t) => String -> Saver s t -> History s -> IO ()
saveT filename saver history = do
  msp $ ("save history size (binary)", length $ toList history)
  let fileContentsString = show (saver <$> history)
  writeFile filename fileContentsString

saveB :: (Binary t, Show t) => String -> Saver s t -> History s -> IO ()
saveB filename saver history = do
  msp $ ("save history size (binary)", length $ toList history)
  let reps = saver <$> history
  encodeFile filename reps
