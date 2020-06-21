{-# LANGUAGE NamedFieldPuns #-}

module Config (config, Config(..)) where

import System.Environment (getArgs)
import System.IO.Unsafe

import Util

data Config = Config { projectDir :: String
                     , args :: [String] } deriving Show

config :: Config
config = unsafePerformIO $ do
  (projectDir:args) <- getArgs
  return $ esp $ Config { projectDir, args }
