module External
( readFromProc
, runProc
--, jsonCommand
, cachedReadFromProc
, cachedJsonCommand
) where

import Data.Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.ByteString.UTF8 as BSU (fromString, toString)
import Data.Time
import qualified Crypto.Hash.MD5 as MD5
import System.Directory
import System.IO
import System.Process

import Memoize
import Util

verbose = False

-- _readFromProc exe args = vrah (exe, args)
-- vrah = diskMemoize "pleh" lala
-- lala = returnsString oof
-- oof :: (String, [String]) -> IO String
-- oof (exe, args) = readFromProc' exe args

cachedReadFromProc exe args = readTheFile $ (diskMemoize "readFromProc" (returnsString f)) (exe, args)
  where f (exe, args) = readFromProc exe args
        readTheFile :: IO String -> IO String
        readTheFile m = do
          filename <- m
          s <- readFile filename
          return s

readFromProc :: String -> [String] -> IO String
readFromProc exe args = do
  start <- getCurrentTime
  let cp = (proc exe args) { std_out = CreatePipe }
  (_, Just out, _, _) <- createProcess cp
  output <- hGetContents out
  end <- getCurrentTime
  if verbose then msp (show (diffUTCTime end start) ++ " " ++ exe ++ " " ++ (show args)) else return ()
  return output

-- Just ignore the output
runProc exe args = do
  output <- readFromProc exe args
  msp output
  return ()

cachedJsonCommand exe args = do
  rawOutput <- cachedReadFromProc exe args
  return $ (decode (BLU.fromString rawOutput) :: Maybe Value)

-- cachedJsonCommand exe args = decodeResult $ (diskMemoize "cachedJsonCommand" (returnsString f)) (exe, args)
--   where f (exe, args) = jsonCommand exe args
--         decodeResult :: IO String -> IO (Maybe Value)
--         decodeResult m = do
--           rawOutput <- m
--           msp "raw"
--           putStrLn rawOutput
--           return $ (decode (BLU.fromString rawOutput) :: Maybe Value)
-- geh (exe, args) = jsonCommand exe args

md5 :: Show a => a -> String
--md5 x = BSU.toString $ C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)
md5 x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)

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
