module Memoize
( DiskAction(..)
, diskMemoize
, returnsString ) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 as BSU (fromString, toString)
import System.Directory
import System.IO.Temp

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
                    tmp <- emptySystemTempFile "src.wav"
                    f tmp args
                    renameFile tmp filename
                    return filename

-- returnsValue :: Show b => (a -> IO b) -> DiskAction a
-- returnsValue f = TakesFile tf
--   where tf filename a = do
--           b <- f a
--           msp $ ("write it", filename, (show b))
--           writeFile filename (show b)

returnsString :: (a -> IO String) -> DiskAction a
returnsString f = TakesFile tf
  where tf filename a = do
          b <- f a
          --msp $ ("write it", filename, (show b))
          writeFile filename b

md5 :: Show a => a -> String
md5 x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)
