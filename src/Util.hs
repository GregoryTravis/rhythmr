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
, noBuffering
, die
, predSplit
) where

import Control.Exception
import Data.List (groupBy)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Typeable (typeOf)
import System.Exit (die)
import System.IO (appendFile, hFlush, stdout, stderr, hSetBuffering, BufferMode(..))
import System.IO.Unsafe
import Text.Pretty.Simple (pShowNoColor)
import Text.Printf

esp a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ a
  return a

eesp s a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ s
  hFlush stdout
  return a

fesp f a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ f a
  return a

lesp logFile a = leesp logFile (evalString $ show a) a

leesp logFile s a = unsafePerformIO $ do
  appendFile logFile (evalString $ show s ++ "\n")
  return a

eeesp s a = unsafePerformIO $ do
  putStrLn $ evalString $ show $ (s, a)
  return a

-- Fake ones for quickly disabling
feesp s a = a

evalString s = seq (length s) s

sp x = unpack $ toStrict $ pShowNoColor $ x
--sp x = show x
msp x = putStrLn $ evalString $ sp x

tsp x = putStrLn $ (sp x) ++ " :: " ++ (sp (typeOf x))
ttsp x = putStrLn $ "_ :: " ++ (sp (typeOf x))
tesp x = eesp (sp x ++ " :: " ++ (sp (typeOf x))) x
ttesp x = eesp ("_ :: " ++ (sp (typeOf x))) x

-- Really surprised this doesn't exist
fromLeftReal (Left a) = a

massert :: Show m => m -> Bool -> IO ()
massert m b = do let _ = assert b ()
                 -- And again in case they're turned off
                 if not b
                   then throw $ AssertionFailed ("Assertion Failed: " ++ (show m))
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
    start <- getSystemTime
    v <- a
    end <- getSystemTime
    let diff = (systemToUTCTime end) `diffUTCTime` (systemToUTCTime start)
    printf "%s %s\n" s (show diff)
    return v

noBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

predSplit p xs = groupBy same xs
  where same a b = p a == p b
