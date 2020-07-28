module Memoize
( DiskAction(..)
, diskMemoize
, returnsString
, memoizeIO
, memoizePure
, emptyMemoDir ) where

import Data.IORef
import qualified Data.Map.Strict as MS
import System.Directory
import System.IO.Temp
import System.IO.Unsafe (unsafePerformIO)

import qualified Hash as H
import Util

memoDir = ".memo"

ensureMemoDir :: IO ()
ensureMemoDir = createDirectoryIfMissing False memoDir

-- String: filename
-- a: other args
data DiskAction a = TakesFile (String -> a -> IO ())

-- Turn a disk action into a memoized function that returns a filename
diskMemoize :: Show a
            => String  -- a label, used in the .memo filename
            -> DiskAction a  -- an action that also takes the memoization filename; it should put its output there
            -> (a -> IO String)  -- an action taking an argument
diskMemoize functionName (TakesFile f) args = do
  let key = show (functionName, args)
      hash = H.hash key
      filename = memoDir ++ "/" ++ functionName ++ "-" ++ hash
  exists <- doesFileExist filename
  -- factor out return filename
  if exists then do msp $ "cache hit " ++ key
                    return filename
            else do ensureMemoDir
                    msp $ "cache miss " ++ key
                    tmp <- emptySystemTempFile "src.wav"
                    f tmp args
                    msp ("UGH4", filename)
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

memoizePure :: Ord a => (a -> b) -> IO (a -> b)
memoizePure f = do
  ioref <- newIORef MS.empty
  let memoizedF = \a -> unsafePerformIO $ do
        cache <- readIORef ioref
        -- case MS.lookup a cache of Just b -> msp "cache hit"
        --                           Nothing -> msp "cache miss"
        case MS.lookup a cache of Just b -> return b
                                  Nothing -> do let b = f a
                                                writeIORef ioref $ MS.insert a b cache
                                                return b
  return memoizedF

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

emptyMemoDir :: IO ()
emptyMemoDir = do
  b <- doesFileExist memoDir
  if b then removeDirectoryRecursive memoDir
       else return ()
