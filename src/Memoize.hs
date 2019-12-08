module Memoize
( DiskAction(..)
, diskMemoize ) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 as BSU (fromString, toString)
import System.Directory

import Util

memoDir = ".memo"

-- Two kinds of disk-based actions to memoize:
-- - One that returns a filename
-- - One that takes a filename

data DiskAction a = TakesFile (String -> a -> IO ()) | GivesFile (a -> IO String)

-- Turn a disk action into a memoized function that returns a filename
diskMemoize :: Show a => String -> DiskAction a -> (a -> IO String)
diskMemoize functionName (TakesFile f) args = do
  let key = show (functionName, args)
      hash = md5 key
      filename = memoDir ++ "/" ++ functionName ++ "-" ++ hash
  exists <- doesFileExist filename
  -- factor out return filename
  if exists then do msp $ "cache hit " ++ key
                    return filename
            else do msp $ "cache miss " ++ key
                    f filename args
                    return filename

md5 :: Show a => a -> String
md5 x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)

-- diskMemoize :: IO Bool -> a -> IO a -> IO a
-- diskMemoize check cachedResult get = do
--   gotten <- check
--   if gotten
--      then do msp "cache hit"
--              return cachedResult
--      else do msp "cache miss"
--              get

-- download id = diskMemoize check outputFilename get
--   where outputFilename = downloadDir ++ "/a-" ++ id ++ ".wav"
--         outputTemplate = downloadDir ++ "/a-%(id)s.%(ext)s"
--         check = doesFileExist outputFilename
--         get = do runProc "youtube-dl" ["-o", outputTemplate, "-x", "--audio-format", "wav", "https://www.youtube.com/watch?v=" ++ id]
--                  return outputFilename
