{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Stow
( stowMain ) where

import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe

import Hash
import Util

stowDir :: String
stowDir = ".stow"

type Key = String

class Show a => Stowable a where
  stow :: a -> Key
  stow = show

-- Maybe this should be the reverse?
-- instance Show a => Stowable a where
--   stow = show

data StoredIndirect = StoredIndirect Key
  deriving Show

data Indirect a = Indirect Key (Maybe a)
instance Show (Indirect a) where
  show (Indirect key _) = show (StoredIndirect key)

indirect :: (Stowable a) => a -> Indirect a
indirect x =
  let s = stow x
      key = hash s
      filename = makeFilename key
      writeIt = writeFile filename s
   in (unsafePerformIO writeIt) `seq` Indirect key (Just x)

makeFilename :: Key -> FilePath
makeFilename key = (unsafePerformIO ensureDir) `seq` (stowDir ++ "/" ++ key)

ensureDir :: IO ()
ensureDir = createDirectoryIfMissing True stowDir

saveRoot :: Stowable a => a -> Indirect a
saveRoot = indirect

data W = W { ints :: [Int]
           , string :: String }
  deriving Show

instance Stowable W

stowMain = do
  let w = W { ints = [1, 2], string = "asdf" }
  msp $ saveRoot w
  --msp $ stow w
  msp "hi"
