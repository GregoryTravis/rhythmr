module FPS
( fps
, fpsAction2 ) where

import Control.Concurrent.MVar
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime, SystemTime)
import System.IO.Unsafe (unsafePerformIO)

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
fpsAction2 :: (x -> y -> IO a) -> IO (x -> y -> IO a)
fpsAction2 action = do
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

fps :: IO ()
fps = unsafePerformIO $ do
  fps' <- fpsAction2 (\_ _ -> return ())
  return $ do
    fps' () ()
