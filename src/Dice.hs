module Dice
( double
, halve
, dice
, eqDice
, rev ) where

import Arrangement
import Sound
import Util

-- This stuff should really be working with arrangements

-- Input must be an Arrangement of just one sound
type Replacer = (Int, Int) -> [(Int, Int)]
rePlace :: Replacer -> Arrangement -> Arrangement
rePlace f (Arrangement [(Placement sound (Span s e))]) = Arrangement (map (uncurry place) (zip ses (repeat sound)))
  where ses = f (s, e)
        place (s, e) sound = Placement sound (Span s e)

double :: Arrangement -> Arrangement
double = rePlace f
  where f (s, e) = [(s, mid), (mid, e)]
          where mid = (s + e) `div` 2

reSnip :: Replacer -> Arrangement -> Arrangement
reSnip f (Arrangement [(Placement sound (Span s e))]) = Arrangement [p]
  where p = Placement newSound (Span s e)
        [(newS, newE)] = f (s, e)
        newSound = snip s e sound

halve :: Arrangement -> Arrangement
halve = reSnip f
  where f (s, e) = [(s, halfE)]
          where halfE = s + ((e - s) `div` 2)

eqDice :: Arrangement -> Int -> Arrangement
eqDice arr n = dice arr ds
  where ds :: [Double]
        ds = map (/fromIntegral n) $ map fromIntegral [0..n]

rev :: Arrangement -> Arrangement
--rev (Arrangement ps) = Arrangement (reverse ps)
rev = switchPlaces reverse

-- Give placement i the span of placement (f i)
switchPlaces :: ([Placement] -> [Placement]) -> Arrangement -> Arrangement
switchPlaces f (Arrangement ps) = Arrangement newPs
  where newPs = zipWith takePlace ps (f ps)

-- Move placement a to the span of placement b
takePlace :: Placement -> Placement -> Placement
takePlace (Placement sound _) (Placement _ span) = Placement sound span

dice :: Arrangement -> [Double] -> Arrangement
dice (Arrangement [(Placement sound (Span s e))]) cutPoints = Arrangement ps
  where snipPoints = esp $ chopInts cutPoints (0, len)
        placePoints = esp $ chopInts cutPoints (s, e)
        ps = zipWith place snipPoints placePoints
        place (snipS, snipE) (placeS, placeE) = Placement (snip snipS snipE sound) (Span placeS placeE)
        len = numFrames sound

-- dice (Arrangement [(Placement sound (Span s e))]) cutPoints | length cutPoints < 2 = error "Need >1 cutpoints"
-- dice (Arrangement [(Placement sound (Span s e))]) cutPoints | otherwise = Arrangement (map (snipIt sound) snips)
--   where snipsF = zip cutPoints (tail cutPoints)
--         snips = map toInts snipsF
--         toInts :: (Double, Double) -> (Int, Int)
--         toInts (s, e) = (floor (fromIntegral len * s), floor (fromIntegral len * e))
--         len = numFrames sound
--         snipIt sound (s, e) = snip s e sound

-- Map a list of 0..1 points to an integer range, as pairs
chopInts :: [Double] -> (Int, Int) -> [(Int, Int)]
chopInts ds (s, e) = toSpans (map interp ds)
  where interp d = s + floor (d * fromIntegral (e - s))

-- Form spans between successive values
toSpans :: [a] -> [(a, a)]
toSpans xs | length xs <= 1 = error "toSpans: too few"
toSpans xs | otherwise = zip xs (tail xs)
