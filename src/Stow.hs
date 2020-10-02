{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- error if the file exists

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

data StoredIndirect a = StoredIndirect Key
  deriving (Read, Show)

data Indirect a = Indirect Key (Maybe a)
instance Show (Indirect a) where
  show (Indirect key _) = show (StoredIndirect key)
instance Read (Indirect a) where
  readsPrec prec s = map unSI $ (readsPrec prec s :: [(StoredIndirect a, String)])
    where unSI (StoredIndirect key, rest) = (Indirect key Nothing, rest)

keyOf :: Indirect a -> Key
keyOf (Indirect key _) = key

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

saveRoot :: Stowable a => a -> Key
saveRoot = keyOf . indirect

loadRoot :: Read a => Key -> a
loadRoot key = case retrieve (Indirect key Nothing) of Indirect _ (Just x) -> x

retrieve :: Read a => Indirect a -> Indirect a
--retrieve (Indirect key (Just x)) = x
retrieve (Indirect key Nothing) = x `seq` (Indirect key (Just x))
  where x = unsafePerformIO $ do
          s <- ((readFile (makeFilename key)) :: IO String)
          return $ read s
retrieve ind = ind

data W = W { ints :: [Int]
           , string :: String }
  deriving (Read, Show)

instance Stowable W

stowMain = do
  let w = W { ints = [1, 2], string = "asdf" }
  let saved = saveRoot w
  msp saved
  let w' :: W
      w' = loadRoot saved
  msp w'
  msp "hi"
