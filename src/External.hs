module External
( readFromProc
) where

import System.IO
import System.Process

readFromProc :: String -> [String] -> IO String
readFromProc exe args = do
  let cp = (proc exe args) { std_out = CreatePipe }
  (_, Just out, _, _) <- createProcess cp
  hGetContents out
