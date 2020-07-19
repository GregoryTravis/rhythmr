module Project (getProjectFile, getLoopDir, getLoopDirs, cleanupProjectDir) where

import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath.Posix (dropTrailingPathSeparator)

import Util

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True

getProjectFile :: FilePath -> IO FilePath
getProjectFile projectDir = do
  ensureDir projectDir
  return $ projectDir ++ "/history"

getLoopDir :: FilePath -> String -> IO FilePath
getLoopDir projectDir collection = do
  let loopDir = projectDir ++ "/loops/" ++ collection
  ensureDir loopDir
  return loopDir

getLoopDirs :: FilePath -> IO [FilePath]
getLoopDirs projectDir = do
  ensureDir projectDir
  dirs <- listDirectory (projectDir ++ "/loops/")
  return dirs

cleanupProjectDir :: FilePath -> FilePath
cleanupProjectDir = dropTrailingPathSeparator
