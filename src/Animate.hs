module Animate
( AVal(..)
, Interpolator(..)
, readAVal
, updateAVal
, constAVal
, aValSize
) where

import qualified Data.Map.Strict as M
import qualified Debug.Trace as TR

import Util

duration = 0.5

data AVal a = Const a | Blend Float Float (AVal a) (AVal a) (Interpolator a)

instance Show a => Show (AVal a) where
  show (Const a) = "<" ++ (show a) ++ ">"
  show (Blend s e a a' _) = "<" ++ show (s, e, a, a') ++ ">"

data Interpolator a = Interpolator (Float -> Float -> Float -> a -> a -> a)

applyInterpolator (Interpolator f) = f

aValSize :: AVal a -> Int
aValSize (Const _) = 1
aValSize (Blend _ _ old new _) = (aValSize old) + (aValSize new)

updateAVal :: (Show a, Eq a) => Float -> AVal a -> a -> Interpolator a -> AVal a
updateAVal t aval a interp = gcAVal t $ if theSame then aval else blended
  where s = t
        e = t + duration
        theSame = case aval of (Const oa) -> oa == a
                               _ -> False
        --(oa, _) = readAVal aval t
        blended = Blend s e aval (constAVal a) interp

constAVal :: a -> AVal a
constAVal a = Const a

gcAVal :: Show a => Float -> AVal a -> AVal a
gcAVal t a@(Blend s e old new interp) | e <= t = eesp ("gc", old, new) $ gcAVal t new
gcAVal t a@(Blend s e old new interp) | otherwise = Blend s e (gcAVal t old) (gcAVal t new) interp
gcAVal t a@(Const _) = a

--readAVal a t | TR.trace (show ("rSA", a, t)) False = undefined
readAVal a t = readAVal' a t

readAVal' :: Show a => AVal a -> Float -> a
readAVal' (Const a) _ = a
readAVal' (Blend s e old new interp) t = a -- | s <= t && t < e = a
  where oa = readAVal' old t
        na = readAVal' new t
        a = applyInterpolator interp t s e oa na
