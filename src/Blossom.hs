module Blossom
( blossomMain ) where

import Control.Monad (replicateM)

import FX
import Loop
import Project
import Rand
import Util
import ZoundCAW
import Zounds

blossomMain :: FilePath -> String -> String -> Int -> IO ()
blossomMain projectDir srcCollection destCollection count = do
  srcDir <- getLoopDir projectDir srcCollection
  destDir <- getLoopDir projectDir destCollection
  blossomDir srcDir destDir count

blossomDir :: FilePath -> FilePath -> Int -> IO ()
blossomDir srcDir destDir numToGenerate = do
  srcFiles <- listDirectoryWithPath srcDir
  runList_ $ take numToGenerate $ zipWith ($) (randListParam (blossomFile destDir) srcFiles) randFXs

blossomFile :: FilePath -> FilePath -> FX -> IO ()
blossomFile destDir zoundFile fx = do
  z <- readZound zoundFile
  z' <- applyFX fx z
  writeZounds ("loop-" ++ getSourceTrackName (Loop zoundFile)) destDir [z']
  return ()

-- Infinite random list of random effects
randFXs :: [FX]
randFXs = interlaceFlattenInfinite
  [ randParam (800, 2400) Highpass
  , randParam (800, 2400) Lowpass
  , randParam2 (880, 3000) (200, 1200) Band
  , randParam (40, 95) Reverb ]

-- assumes all lists are infinite
interlaceFlattenInfinite :: [[a]] -> [a]
interlaceFlattenInfinite xs = map head xs ++ interlaceFlattenInfinite (map tail xs)
