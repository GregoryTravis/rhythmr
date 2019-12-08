module Download (download) where

import System.Directory

import External
import Memoize
import Util

--download :: String -> 
download = diskMemoize "download" $ TakesFile f
  where f filename id = do
          let outputFilename = "a-" ++ id ++ ".wav"
              outputTemplate = "a-%(id)s.%(ext)s"
          runProc "youtube-dl" ["-o", outputTemplate, "-x", "--audio-format", "wav", "https://www.youtube.com/watch?v=" ++ id]
          renameFile outputFilename filename
