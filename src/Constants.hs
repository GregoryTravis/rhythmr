module Constants where

bpm = 120
meter = 4
loopLengthSeconds = (60.0 / fromInteger bpm) * meter
standardSR = 44100
loopLengthFrames :: Int
loopLengthFrames = floor $ (fromInteger standardSR) * loopLengthSeconds
