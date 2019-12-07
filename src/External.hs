module External
( readFromProc
, jsonCommand
) where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import System.IO
import System.Process

readFromProc :: String -> [String] -> IO String
readFromProc exe args = do
  let cp = (proc exe args) { std_out = CreatePipe }
  (_, Just out, _, _) <- createProcess cp
  hGetContents out

jsonCommand exe args = do
  rawOutput <- readFromProc exe args
  return $ (decode (BLU.fromString rawOutput) :: Maybe Value)
