module Constants where

bmp = 120
meter = 4
loopLengthSeconds = (60.0 / fromInteger bmp) * meter
standardSR = 44100
loopLengthFrames :: Int
loopLengthFrames = floor $ (fromInteger standardSR) * loopLengthSeconds
