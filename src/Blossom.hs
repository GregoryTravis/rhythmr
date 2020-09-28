module Blossom
( blossomDir ) where

import Control.Monad (replicateM)
import System.Directory (listDirectory)

import FX
import Loop
import Rand
import Util
import ZoundCAW
import Zounds

blossomDir :: FilePath -> FilePath -> Int -> IO ()
blossomDir srcDir destDir numToGenerate = do
  srcFiles <- listDirectory srcDir
  runList_ $ take numToGenerate $ randListParam2 (blossomFile destDir) srcFiles randFXs

--writeZounds :: String -> FilePath -> [Zound] -> IO [String]

blossomFile :: FilePath -> FilePath -> FX -> IO ()
blossomFile destDir zoundFile fx = do
  z <- readZound zoundFile
  z' <- applyFX fx z
  writeZounds (getSourceTrackName (Loop zoundFile)) destDir [z']
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
