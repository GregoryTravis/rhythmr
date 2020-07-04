module Hash (hash) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 as BSU (fromString)

hash :: Show a => a -> String
hash x = C8.unpack $ B16.encode $ MD5.finalize $ MD5.update MD5.init (BSU.fromString $ show x)
