module External
( readFromProc
, runProc
, runViaFiles
, runViaFilesCmd
--, jsonCommand
, csvCommand
, cachedReadFromProc
, cachedJsonCommand
, withContentHashNamedFile
, contentAddressableWrite
) where

import Control.Exception (evaluate)
import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Time
import System.Directory
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Temp
import System.Process

import Memoize
import Util

verbose = False

-- _readFromProc exe args = vrah (exe, args)
-- vrah = diskMemoize "pleh" lala
-- lala = returnsString oof
-- oof :: (String, [String]) -> IO String
-- oof (exe, args) = readFromProc' exe args

cachedReadFromProc :: String -> [String] -> IO String
cachedReadFromProc exe args = readTheFile $ (diskMemoize "readFromProc" (returnsString f)) (exe, args)
  where f (exe, args) = readFromProc exe args
        readTheFile :: IO String -> IO String
        readTheFile m = do
          filename <- m
          s <- readFile filename
          return s

readFromProc :: String -> [String] -> IO String
readFromProc exe args = do
  -- msp $ intercalate " " ([exe] ++ args)
  start <- getCurrentTime
  let cp = (proc exe args) { std_out = CreatePipe }
  (stdin, Just stdout, stderr, processHandle) <- createProcess cp
  output <- hGetContents stdout
  evaluate $ length output
  exitCode <- waitForProcess processHandle
  massert "proc exit code" (exitCode == ExitSuccess)
  end <- getCurrentTime
  if verbose then msp (show (diffUTCTime end start) ++ " " ++ exe ++ " " ++ (show args)) else return ()
  return output

-- Just ignore the output
runProc exe args = do
  output <- readFromProc exe args
  --msp output
  return ()

runProcArr :: [String] -> IO ()
runProcArr (exe : args) = {- eesp (exe, args) $ -} runProc exe args

withTmp :: String -> (String -> IO a) -> IO a
withTmp ext action = do
  withSystemTempFile ("rhythmr." ++ ext) callback
  where callback filePath handle = action filePath

-- TODO surely this is a fold
withTmps :: String -> Int -> ([String] -> IO a) -> IO a
withTmps ext 0 action = action []
withTmps ext n action = do
  withTmp ext (\filename -> withTmps ext (n-1) (\filenames -> action (filename : filenames)))

-- Write the arg to a file, run a command to produce another file from it, and
-- read the result, cleaning up all intermediate files.
runViaFiles :: String -> (String -> a -> IO ()) -> (String -> IO b) -> (String -> String -> IO ()) -> (a -> IO b)
runViaFiles ext writer reader commander x = do
  withTmps ext 2 $ \[src, dest] -> do
    writer src x
    commander src dest
    y <- reader dest
    return y

runViaFilesCmd :: String -> (String -> a -> IO ()) -> (String -> IO b) -> (String -> String -> [String]) -> (a -> IO b)
runViaFilesCmd ext writer reader commandBuilder = runViaFiles ext writer reader commander
  where commander f0 f1 = runProcArr (commandBuilder f0 f1)

cachedJsonCommand :: String -> [String] -> IO (Maybe Value)
cachedJsonCommand exe args = do
  rawOutput <- (cachedReadFromProc exe args) :: IO String
  return $ (decode (BLU.fromString rawOutput) :: Maybe Value)

csvCommand :: String -> [String] -> IO [[String]]
csvCommand exe args = do
  rawOutput <- cachedReadFromProc exe args
  return $ map uncomma (lines rawOutput)
  where uncomma = splitOn ","

-- cachedJsonCommand exe args = decodeResult $ (diskMemoize "cachedJsonCommand" (returnsString f)) (exe, args)
--   where f (exe, args) = jsonCommand exe args
--         decodeResult :: IO String -> IO (Maybe Value)
--         decodeResult m = do
--           rawOutput <- m
--           msp "raw"
--           putStrLn rawOutput
--           return $ (decode (BLU.fromString rawOutput) :: Maybe Value)
-- geh (exe, args) = jsonCommand exe args

--md5 :: Show a => a -> String
----md5 x = BSU.toString $ C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)
--md5 x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)

-- _cachedReadFromProc exe args = do
--   createDirectoryIfMissing False dir
--   let memoFilename = dir ++ "/" ++ hash
--   exists <- doesFileExist memoFilename
--   if exists
--      then do -- msp "cache hit"
--              readFile memoFilename
--      else do -- msp "cache miss"
--              output <- readFromProc exe args
--              writeFile memoFilename output
--              return output
--   where dir = ".memo"
--         hash = md5 (exe, args)

_cachedJsonCommand exe args = do
  rawOutput <- cachedReadFromProc exe args
  return $ (decode (BLU.fromString rawOutput) :: Maybe Value)

-- This should be in memoize, but then there's a circular dependency
withContentHashNamedFile :: String -> (String -> IO ()) -> (String -> IO a) -> IO a
withContentHashNamedFile label fileWriter action = do
  tmp <- emptySystemTempFile label
  tmpDir <- getCanonicalTemporaryDirectory
  fileWriter tmp
  hashWithNewline <- readFromProc "md5" ["-q", tmp]
  let hash = chomp hashWithNewline
      newPath = tmpDir ++ "/" ++ label ++ "-" ++ hash
  msp ("UGH2", newPath)
  renameFile tmp newPath
  a <- action newPath
  removeFile newPath
  return a

-- Takes a destination directory and a file writer and returns the final
-- location of the file
contentAddressableWrite :: String -> String -> String -> (String -> IO ()) -> IO String
contentAddressableWrite label destDir ext fileWriter = do
  tmp <- emptySystemTempFile label
  fileWriter tmp
  hashWithNewline <- readFromProc "md5" ["-q", tmp]
  let hash = chomp hashWithNewline
      newPath = destDir ++ "/" ++ label ++ "-" ++ hash ++ "." ++ ext
  msp ("UGH3", newPath)
  renameFile tmp newPath
  return newPath
