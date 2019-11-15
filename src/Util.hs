module Util
( assert
, assertM
, massert
, esp
, eesp
, fesp
, lesp
, leesp
, eeesp
, feesp
, sp
, msp
, tsp
, ttsp
, tesp
, ttesp
, fromLeftReal
, mappily
, mcompose
, time
) where

import Control.Exception
import Control.Exception.Base
import Data.Either
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Typeable (typeOf)
import System.CPUTime
import System.IO (appendFile, hFlush, stdout)
import System.IO.Unsafe
import Text.Pretty.Simple (pShow, pShowNoColor)
import Text.Printf

esp a = unsafePerformIO $ do
  putStrLn $ show $ a
  return a

eesp s a = unsafePerformIO $ do
  putStrLn $ show $ s
  hFlush stdout
  return a

fesp f a = unsafePerformIO $ do
  putStrLn $ show $ f a
  return a

lesp logFile a = leesp logFile (show a) a

leesp logFile s a = unsafePerformIO $ do
  appendFile logFile (show s ++ "\n")
  return a

eeesp s a = unsafePerformIO $ do
  putStrLn $ show $ (s, a)
  return a

-- Fake ones for quickly disabling
feesp s a = a

sp x = unpack $ toStrict $ pShowNoColor $ x
--sp x = show x
msp x = putStrLn $ sp x

tsp x = putStrLn $ (sp x) ++ " :: " ++ (sp (typeOf x))
ttsp x = putStrLn $ "_ :: " ++ (sp (typeOf x))
tesp x = eesp (sp x ++ " :: " ++ (sp (typeOf x))) x
ttesp x = eesp ("_ :: " ++ (sp (typeOf x))) x

-- Really surprised this doesn't exist
fromLeftReal (Left a) = a

massert :: Bool -> IO ()
--massert b = return $ assert b ()
massert b = do let _ = assert b ()
               -- And again in case they're turned off
               if not b
                 then throw $ AssertionFailed "Assertion Failed"
                 else return ()

assertM :: Show b => b -> Bool -> a -> a
assertM m b a
  | b = a
  | otherwise = unsafePerformIO $ do
      putStrLn $ show m
      return $ assert b a
      --return a

-- I am giving this a terrible name because I know it must exist but I don't
-- know enough to know what it's called and I refused to accept at the moment
-- that it might be called fmap.
mappily :: (a -> b) -> Maybe a -> Maybe b
mappily f (Just x) = Just (f x)
mappily f Nothing = Nothing

mcompose :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
mcompose f g x = case g x of Just y -> f y
                             Nothing -> Nothing

-- Taken from https://wiki.haskell.org/Timing_computations
time :: String -> IO t -> IO t
time s a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    --printf "%s %0.3f sec\n" s (diff :: Double)
    printf "%s %f sec\n" s (diff :: Double)
    return v
