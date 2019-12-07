module External
( readFromProc
, jsonCommand
, cachedReadFromProc
, cachedJsonCommand
) where

import Data.Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.ByteString.UTF8 as BSU (fromString, toString)
import qualified Crypto.Hash.MD5 as MD5
import System.Directory
import System.IO
import System.Process

import Util

readFromProc :: String -> [String] -> IO String
readFromProc exe args = do
  let cp = (proc exe args) { std_out = CreatePipe }
  (_, Just out, _, _) <- createProcess cp
  hGetContents out

jsonCommand exe args = do
  rawOutput <- readFromProc exe args
  return $ (decode (BLU.fromString rawOutput) :: Maybe Value)

md5 :: Show a => a -> String
--md5 x = BSU.toString $ C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)
md5 x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)

cachedReadFromProc exe args = do
  createDirectoryIfMissing False dir
  let memoFilename = dir ++ "/" ++ hash
  exists <- doesFileExist memoFilename
  if exists
     then do -- msp "cache hit"
             readFile memoFilename
     else do -- msp "cache miss"
             output <- readFromProc exe args
             writeFile memoFilename output
             return output
  where dir = ".memo"
        hash = md5 (exe, args)

cachedJsonCommand exe args = do
  rawOutput <- cachedReadFromProc exe args
  return $ (decode (BLU.fromString rawOutput) :: Maybe Value)
