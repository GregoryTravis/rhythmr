module Download (download) where

import System.Directory

import External
import Memoize
import Util

-- yt-dlp -f ba --extract-audio --audio-format wav "https://www.youtube.com/watch?v=fAvfAR0XAH0"

download = diskMemoize "download" $ TakesFile f
  where f filename id = do
          let outputFilename = "a-" ++ id ++ ".wav"
              outputTemplate = "a-%(id)s.%(ext)s"
          runProc "yt-dlp" ["-o", outputTemplate, "-f", "ba", "--extract-audio", "--audio-format", "wav", "https://www.youtube.com/watch?v=" ++ id]
          msp ("UGG", filename)
          renameFile outputFilename filename
