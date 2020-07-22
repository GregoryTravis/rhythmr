module Project (getProjectFile, getLoopDir, getLoopDirs, initProject) where

import Data.List (isSuffixOf)
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist)
import System.FilePath.Posix (dropTrailingPathSeparator)

import Util

suffix = ".rhythmr"

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True

-- If the specified project does not exist, then add the suffix (if not there)
-- and create it if it doesn't exist.
ensureProjectDir :: FilePath -> IO FilePath
ensureProjectDir projName = do
  b <- doesFileExist projName
  if b
    then return projName
    else do let projName' = addSuffixIfNone projName
            createDirectoryIfMissing True projName'
            createDirectoryIfMissing True (projName' ++ "/loops")
            return projName'

getProjectFile :: FilePath -> IO FilePath
getProjectFile projectDir = do
  --ensureDir projectDir
  return $ projectDir ++ "/history"

getLoopDir :: FilePath -> String -> IO FilePath
getLoopDir projectDir collection = do
  let loopDir = projectDir ++ "/loops/" ++ collection
  ensureDir loopDir
  return loopDir

getLoopDirs :: FilePath -> IO [FilePath]
getLoopDirs projectDir = do
  --ensureDir projectDir
  dirs <- listDirectory (projectDir ++ "/loops/")
  return dirs

addSuffixIfNone :: FilePath -> FilePath
addSuffixIfNone s =
  if isSuffixOf suffix s
    then s
    else s ++ suffix

-- Remove trailing slash, in any.
-- Add .rhythmr extension, if not there.
cleanupProjectDir :: FilePath -> FilePath
cleanupProjectDir = dropTrailingPathSeparator

-- Create project if it doesn't exist.
-- - Remove trailing slash.
-- - If the supplied path exists, use it
-- - Otherwise, add .rhythmr (if not there) and create dir
-- Return the actual project path
initProject :: String -> IO FilePath
initProject s = do
  let path = cleanupProjectDir s
  ensureProjectDir path
