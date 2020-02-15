module FPS (fps) where

import Control.Concurrent.MVar
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime, SystemTime)

import Util

-- Doesn't exist in this version?
--nominalDiffTimeToSeconds :: NominalDiffTime -> Float
--nominalDiffTimeToSeconds ndt = eesp (show (ndt, (length (reverse (show ndt))))) 3.4
--nominalDiffTimeToSeconds = read . reverse . esp . tail . reverse . show . show
nominalDiffTimeToSeconds s =
  let ('s' : s') = reverse (show s)
      (s'') = reverse s'
   in read s''


fpsBufferSize = 30
fps :: (x -> y -> IO a) -> IO (x -> y -> IO a)
fps action = do
  now <- getSystemTime
  mvar <- newMVar (take fpsBufferSize $ repeat now)
  let wrapped x y = do
        buffer <- takeMVar mvar
        now <- getSystemTime
        let diff :: Float
            diff = nominalDiffTimeToSeconds $ (systemToUTCTime now) `diffUTCTime` (systemToUTCTime (head buffer))
            avgDiff = diff / (fromIntegral fpsBufferSize)
            fps = 1.0 / avgDiff
            newBuffer = (tail buffer) ++ [now]
        putMVar mvar newBuffer
        msp $ "FPS " ++ show fps
        action x y
  return wrapped
