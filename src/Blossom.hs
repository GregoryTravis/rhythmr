module Blossom
( blossomMain ) where

import Control.Monad (replicateM)

import Chew
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
  msp $ take 10 sprinkles
  runList_ $ take numToGenerate $ zipWith ($) (zipWith ($) (randListParam (blossomFile destDir) srcFiles) randFXs) sprinkles

blossomFile :: FilePath -> FilePath -> FX -> [Int] -> IO ()
blossomFile destDir zoundFile fx spr = do
  z <- readZound zoundFile
  z' <- applyFX fx z
  z'' <- render $ sprinkle (length spr) spr z
  writeZounds ("loop-" ++ getSourceTrackName (Loop zoundFile)) destDir [z'']
  return ()

sprinkles :: [[Int]]
sprinkles = zipWith makeSprinkle (randListChoice [2, 4, 8, 16]) (randDoubles (0, 1))
--sprinkles = randParam (0, 1) (randListParam makeSprinkle [4, 8, 16])

-- Generate a sprinkle of length n with the specified percentage of present pieces
-- This uses the same stream of random percentile rolls each time, which is probably boring.
makeSprinkle :: Int -> Double -> [Int]
makeSprinkle n prob = zipWith r [0..n-1] (randBools prob)
  where r :: Int -> Bool -> Int
        r i True = i
        r _ False = -1

--sprinkle :: Int -> [Int] -> Zound -> Zound

-- Infinite random list of random effects
randFXs :: [FX]
randFXs = interlaceFlattenInfinite
  [ randParam (800, 2400) Highpass
  , randParam (800, 2400) Lowpass
  , randParam2 (880, 3000) (200, 1200) Band
  , randParam (40, 95) Reverb
  , r Chorus
  , r NoiseGate
  , r Squelch
  , r Flange
  , r MCompand
  , r Phaser
  , r Reverse
  , randParam (150, 700) Echo
  , randParam2 (15, 25) (15, 25) Overdrive
  , randParam (-1200, 1200) Pitch
  , randParam2 (2, 10) (20, 60) Tremolo
  ]
  where r = repeat

-- assumes all lists are infinite
interlaceFlattenInfinite :: [[a]] -> [a]
interlaceFlattenInfinite xs = map head xs ++ interlaceFlattenInfinite (map tail xs)
