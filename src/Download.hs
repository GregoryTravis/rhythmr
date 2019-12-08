module Download (download) where

import System.Directory

import External
import Util

downloadDir = "downloads"

diskMemoize :: IO Bool -> a -> IO a -> IO a
diskMemoize check cachedResult get = do
  gotten <- check
  if gotten
     then do msp "cache hit"
             return cachedResult
     else do msp "cache miss"
             get

download id = diskMemoize check outputFilename get
  where outputFilename = downloadDir ++ "/a-" ++ id ++ ".wav"
        outputTemplate = downloadDir ++ "/a-%(id)s.%(ext)s"
        check = doesFileExist outputFilename
        get = do runProc "youtube-dl" ["-o", outputTemplate, "-x", "--audio-format", "wav", "https://www.youtube.com/watch?v=" ++ id]
                 return outputFilename
