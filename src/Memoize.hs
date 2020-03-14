module Memoize
( DiskAction(..)
, diskMemoize
, returnsString
, memoizeIO ) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import qualified Data.Map.Strict as MS
import Data.ByteString.UTF8 as BSU (fromString)
import System.Directory
import System.IO.Temp

import Util

memoDir = ".memo"

-- String: filename
-- a: other args
data DiskAction a = TakesFile (String -> a -> IO ())

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

memoizeIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoizeIO f = do
  ioref <- newIORef MS.empty
  let memoizedF a = do
        cache <- readIORef ioref
        -- case MS.lookup a cache of Just b -> msp "cache hit"
        --                           Nothing -> msp "cache miss"
        case MS.lookup a cache of Just b -> return b
                                  Nothing -> do b <- f a
                                                writeIORef ioref $ MS.insert a b cache
                                                return b
  return memoizedF

md5 :: Show a => a -> String
md5 x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)
