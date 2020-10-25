module Resources
( showResourcePath
, getResource ) where

import System.Directory (doesDirectoryExist)
import System.Environment (getExecutablePath)

import Util

findResourceDir :: IO FilePath
findResourceDir = do
  ep <- getExecutablePath
  let relativeResourceDir = (removeSuffix "MacOS/rhythmr" ep) ++ "Resources"
  rrdExists <- doesDirectoryExist relativeResourceDir
  let result = if rrdExists then relativeResourceDir else "."
  msp $ ("rrd", relativeResourceDir, rrdExists, result)
  return result
--Rhythmr//Rhythmr.app/Contents/MacOS/rhythmr

showResourcePath :: IO ()
showResourcePath = do
  rd <- findResourceDir
  putStrLn $ "Resources: " ++ rd

getResource :: FilePath -> IO FilePath
getResource s = do
  rd <- findResourceDir
  return $ rd ++ "/" ++ s
