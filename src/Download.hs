module Download (download) where

import External
import Util

download id = do
  -- The 'a-' is here because sometimes ids start with '-' which bothers ls
  let outputFilename = "a-" ++ id ++ ".wav"
      outputTemplate = "a-%(id)s.wav"
  runProc "youtube-dl" ["-o", outputTemplate, "-x", "--audio-format", "wav", "https://www.youtube.com/watch?v=" ++ id]
  return outputFilename
