module Animate
( AVal(..)
, AValMap(..)
, Interpolator(..)
, emptyAValMap
, setAVal
, readAVal
, getAllAVals
, gcAValMap
) where

import qualified Data.Map.Strict as M
import qualified Debug.Trace as TR

import Util

-- A state (not mentioned here) produces a set of (id, picture element, position).
-- For two successive states, we construct a function to interpolate between them
-- and can produce interpolated tuples (id, picture element, pos). If a new state
-- arrives while a previous animation is happening, they are blended linearly.
-- An animating function returns a value as well a boolean saying whether or not
-- it is done; when it is done, the last position is converted to a constant.
--
-- Ok I think the interface is: here is the new value of an anim var, as well
-- as an animation function, and what is the current value of the anim var?

-- k is the key/id of a var
-- a is the value to animate
-- No wait, we'd need one for each type
--data Animator k a = Animator (M.Map k (AVal a))

data AValMap k a = AValMap (M.Map k (AVal a)) (Interpolator a)

instance (Show k, Show a) => Show (AValMap k a) where
  show (AValMap m _) = "<<" ++ show m ++ ">>"

data AVal a = Const a | Blend Float Float (AVal a) (AVal a) (Interpolator a)

instance Show a => Show (AVal a) where
  show (Const a) = "<" ++ (show a) ++ ">"
  show (Blend s e a a' _) = "<" ++ show (s, e, a, a') ++ ">"

data Interpolator a = Interpolator (Float -> Float -> Float -> a -> a -> a)

applyInterpolator (Interpolator f) = f

-- getInterpolator :: AVal a -> Interpolator a
-- getInterpolator (Const _) = interp
-- getInterpolator (Blend _ _ _ _ interp) = interp

duration = 0.5

aValSize :: AVal a -> Int
aValSize (Const _) = 1
aValSize (Blend _ _ old new _) = (aValSize old) + (aValSize new)

updateAVal :: (Show a, Eq a) => Float -> AVal a -> a -> Interpolator a -> AVal a
updateAVal t aval a interp = if theSame then aval else blended
  where s = t
        e = t + duration
        theSame = case aval of (Const oa) -> oa == a
                               _ -> False
        --(oa, _) = readSingleAVal aval t
        blended = Blend s e aval (constAVal a) interp

constAVal :: a -> AVal a
constAVal a = Const a

gcAVal :: Show a => Float -> AVal a -> AVal a
gcAVal t a@(Blend s e old new interp) | e <= t = eesp ("gc", old, new) $ gcAVal t new
gcAVal t a@(Blend s e old new interp) | otherwise = Blend s e (gcAVal t old) (gcAVal t new) interp
gcAVal t a@(Const _) = a

gcAValMap :: Show a => Float -> AValMap k a -> AValMap k a
gcAValMap t (AValMap m interp) = AValMap m' interp
  where m' = M.map (gcAVal t) m

--readSingleAVal a t | TR.trace (show (a, t)) False = undefined
readSingleAVal a t = readSingleAVal' a t

readSingleAVal' :: Show a => AVal a -> Float -> a
readSingleAVal' (Const a) _ = a
readSingleAVal' (Blend s e old new interp) t = a -- | s <= t && t < e = a
  where oa = readSingleAVal' old t
        na = readSingleAVal' new t
        a = applyInterpolator interp t s e oa na

emptyAValMap :: Interpolator a -> AValMap k a
emptyAValMap interp = AValMap (M.empty) interp

setAVal :: (Show a, Eq a, Ord k) => Float -> k -> a -> AValMap k a -> AValMap k a
setAVal t k a (AValMap m interp) = AValMap m' interp
  where m' = M.insert k aVal m
        --aVal :: AVal aa
        aVal = case (M.lookup k m) of Nothing -> constAVal a
                                      Just oldAVal -> updateAVal t oldAVal a interp

readAVal :: (Show a, Ord k) => Float -> k -> AValMap k a -> a
readAVal t k (AValMap m interp) = readSingleAVal (m M.! k) t

readAVals :: (Show a, Ord k) => Float -> [k] -> AValMap k a -> [a]
readAVals t ks avm = map (\k -> readAVal t k avm) ks
--readAVals t (k:ks) avm = (readAVal t k avm) : (readAVals t ks avm')
--readAVals t [] avm = ([], avm)

getAllAVals :: (Show a, Ord k) => AValMap k a -> Float -> [(k, a)]
getAllAVals avm@(AValMap m interp) t = eesp (map aValSize (M.elems m)) (zip ks as)
  where ks = M.keys m
        as = readAVals t ks avm
