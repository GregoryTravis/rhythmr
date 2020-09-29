module Rand
( randParam
, randParam2
, randParam2same
, randListChoice
, randListParam
, randListParam2
, randBools
, randDoubles ) where

import System.Random

randParam :: (Int, Int) -> (Int -> f) -> [f]
--randParam range f = zipWith ($) (repeat f) rands
randParam range f = map f rands
  where rands = randomRs range (mkStdGen 37)

randParamD :: (Double, Double) -> (Double -> f) -> [f]
--randParam range f = zipWith ($) (repeat f) rands
randParamD range f = map f rands
  where rands = randomRs range (mkStdGen 37)

randParam2 :: (Int, Int) -> (Int, Int) -> (Int -> Int -> f) -> [f]
--randParam2 range f = zipWith ($) (zipWith ($) (repeat f) rands) rands'
randParam2 range range' f = zipWith f rands rands'
  where rands = randomRs range (mkStdGen 37)
        rands' = randomRs range' (mkStdGen 2036)

randParam2same :: (Int, Int) -> (Int -> Int -> f) -> [f]
randParam2same range f = randParam2 range range f

-- Return a list of randomly-chosen elements from the given list.
randListChoice :: [a] -> [a]
randListChoice xs = randParam (0, length xs-1) (xs !!)

randListParam :: (a -> b) -> [a] -> [b]
randListParam f xs = map f (randListChoice xs)

randListParam2 :: (a -> b -> c) -> [a] -> [b] -> [c]
randListParam2 f xs ys = zipWith f (randListChoice xs) (randListChoice ys)

-- Return an infinite list of bools with the given probability of being true, 0..1
randBools :: Double -> [Bool]
randBools prob = randParamD (0.0, 1.0) (< prob)

randDoubles :: (Double, Double) -> [Double]
randDoubles range = randomRs range (mkStdGen 934)
