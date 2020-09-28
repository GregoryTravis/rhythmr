module ZoundCAW
( writeZounds
) where

import External
import Zounds
import Util

writeZounds :: String -> FilePath -> [Zound] -> IO [String]
writeZounds fileStub dir sounds = mapM (contentAddressableWrite fileStub dir "wav" . writer) sounds
  where writer :: Zound -> String -> IO ()
        writer sound filename = writeZound filename sound
