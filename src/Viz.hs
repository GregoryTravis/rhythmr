{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Viz
  ( stateToViz
  , initViz
  , renderViz
  ) where

import Data.IORef
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
--import Graphics.Gloss.Interface.IO.Game
import Linear
import Linear.V (fromVector)
import System.Exit (exitSuccess)

import Animate
import Gui
import Hypercube
import Loop
import Looper
import Score
import State
import Util

duration = 0.5

gridSizeFor :: Int -> Int
gridSizeFor n = ceiling $ sqrt $ fromIntegral n

-- What are the coordinates of i in a sqaure-ish grid of n values?
toGridXY :: Int -> Int -> V2 Int
toGridXY i n = V2 x y
  where gridSize = gridSizeFor n
        x = i `mod` gridSize
        y = i `div` gridSize

toGridXYF i n = (fmap fromIntegral (toGridXY i n)) / (fmap fromIntegral (V2 gridSize gridSize))
  where gridSize = gridSizeFor n

-- unitSquareTo :: V2 Float -> V2 Float -> (Picture -> Picture)
-- unitSquareTo (V2 llx lly) (V2 w h) picture = Translate llx lly $ Scale w h picture

scaler :: V2 Float -> (V2 Float -> V2 Float)
scaler (V2 x y) (V2 x' y') = V2 (x*x') (y*y')

translater :: V2 Float -> (V2 Float -> V2 Float)
translater (V2 x y) (V2 x' y') = V2 (x+x') (y+y')

gridTransformsForN :: Int -> [V2 Float -> V2 Float]
gridTransformsForN n =
  let gridSize = gridSizeFor n
      ijs = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]
      gbx = V2 gridStep 0
      gby = V2 0 gridStep
      gridStep :: Float
      gridStep = 1.0 / (fromIntegral gridSize)
      translateFor (i, j) = (fromIntegral i *^ gbx) + (fromIntegral j *^ gby)
      scale = V2 gridStep gridStep
      transformFor ij = (translater (translateFor ij)) . (scaler scale)
   in take n (map transformFor ijs)

ringOfCirclesInUnitSquare :: Int -> [V2 Float -> V2 Float]
ringOfCirclesInUnitSquare n = circles
  where circles = map circle [0..n-1]
        circle i = scaler (V2 0.5 0.5) . translater (V2 1 1) . translater offset
          where ang = 2 * pi * (fromIntegral i / fromIntegral n)
                offset = (1.0 - margin - (circleRadius / 2)) *^ V2 (cos ang) (sin ang)
        tr (V2 x y) p = Translate x y p
        circleRadius = 0.15
        margin = 0.55

affinityPositions :: State -> M.Map Loop (V2 Float)
affinityPositions s = case acceptable s of xss -> M.fromList $ concat (zipWith rah (gridTransformsForN (length xss)) xss)
  where rah :: (V2 Float -> V2 Float) -> [Loop] -> [(Loop, V2 Float)]
        rah xform xs = zip xs $ map (\cXform -> ((scaler (V2 400 400)) . xform . cXform) (V2 0 0)) (ringOfCirclesInUnitSquare (length xs))

interpV :: Float -> Float -> Float -> V2 Float -> V2 Float -> V2 Float
interpV t s e (V2 x y) (V2 x' y') = V2 x'' y''
  where x'' = interp t s e x x'
        y'' = interp t s e y y'

interp :: Float -> Float -> Float -> Float -> Float -> Float
--interp t s e a a' = eesp (t, s, e, a, a') $ a + (k * (a' - a))
interp t s e a a' = a + (k * (a' - a))
  where k = clip 0.0 1.0 ((t - s) / (e - s))

clip :: (Ord a, Num a) => a -> a -> a -> a
clip lo hi x | x < lo = lo
clip lo hi x | x > hi = hi
clip lo hi x | otherwise = x

-- Some potential for inconsistency here, Pic and it's Tag could differ
data Tag = LoopT Loop | SeqT Loop Int Float | LoopPlaceT Loop | MarkT Int
  deriving (Eq, Show, Ord)
data Pic c = LoopP Tag (c (V2 Float))
           | SeqP Tag (c (V2 Float)) (c Float)
           | LoopPlaceP Tag (c (V2 Float))
           | MarkP Tag (c (V2 Float))
deriving instance () => Show (Pic AVal)
deriving instance () => Show (Pic Id)

data Id a = Id a
  deriving Show

mapPic :: (forall a . (Eq a, Show a) => c a -> c' a) -> Pic c -> Pic c'
mapPic f (SeqP tag pos size) = SeqP tag (f pos) (f size)
mapPic f (LoopP tag pos) = LoopP tag (f pos)
mapPic f (LoopPlaceP tag pos) = LoopPlaceP tag (f pos)
mapPic f (MarkP tag pos) = MarkP tag (f pos)

zipWithPic :: (forall a . (Eq a, Show a) => c a -> d a -> e a) -> Pic c -> Pic d -> Pic e
zipWithPic f (SeqP tag pos size) (SeqP tag' pos' size') | tag == tag' = SeqP tag (f pos pos') (f size size')
zipWithPic f (LoopP tag pos) (LoopP tag' pos') | tag == tag' = LoopP tag (f pos pos')
zipWithPic f (LoopPlaceP tag pos) (LoopPlaceP tag' pos') | tag == tag' = LoopPlaceP tag (f pos pos')
zipWithPic f (MarkP tag pos) (MarkP tag' pos') | tag == tag' = MarkP tag (f pos pos')

mapSquish :: (forall a . (Eq a, Show a) => c a -> b) -> Pic c -> [b]
mapSquish f (SeqP tag pos size) = [f pos, f size]
mapSquish f (LoopP tag pos) = [f pos]
mapSquish f (LoopPlaceP tag pos) = [f pos]
mapSquish f (MarkP tag pos) = [f pos]

gcReport :: Viz -> [Int]
gcReport (Viz pics) = concat $ map (mapSquish aValSize) pics

getTag :: Pic a -> Tag
getTag (LoopP tag _) = tag
getTag (SeqP tag _ _) = tag
getTag (LoopPlaceP tag _) = tag
getTag (MarkP tag _) = tag

picInterpolator :: Pic c -> Pic Interpolator
picInterpolator (LoopP tag _) =
  LoopP tag v2FloatInterpolator
picInterpolator (SeqP tag _ _) =
  SeqP tag v2FloatInterpolator floatInterpolator
picInterpolator (LoopPlaceP tag _) =
  LoopPlaceP tag v2FloatInterpolator
picInterpolator (MarkP tag _) =
  MarkP tag v2FloatInterpolator

-- Surely this exists already
data Pair c d a = Pair (c a) (d a)
pair :: (c a) -> (d a) -> Pair c d a
pair = Pair

updatePic :: Float -> Float -> Pic AVal -> Pic AVal -> Pic AVal
updatePic t t' old new = mapPic f $ zipWithPic pair (zipWithPic pair old new) (picInterpolator new)
  where f :: (Eq a, Show a) => (Pair (Pair AVal AVal) Interpolator) a -> AVal a
        f (Pair (Pair aval v) interpolator) = updateAVal t t' aval v interpolator

idToAVal :: Id a -> AVal a
idToAVal (Id a) = constAVal a
aValToId :: Show a => Float -> AVal a -> Id a
aValToId t = Id . flip readAVal t

constPic :: Pic Id -> Pic AVal
constPic = mapPic idToAVal

v2FloatInterpolator = Interpolator interpV
floatInterpolator = Interpolator interp
colorInterpolator = Interpolator colorInterpolator'
colorInterpolator' :: Float -> Float -> Float -> Color -> Color -> Color
colorInterpolator' t s e color color' = makeColor r'' g'' b'' a''
  where r'' = interp t s e r r'
        g'' = interp t s e g g'
        b'' = interp t s e b b'
        a'' = interp t s e a a'
        (r, g, b, a) = rgbaOfColor color
        (r', g', b', a') = rgbaOfColor color'

-- pef :: Show (c _) => Pic c
-- pef = undefined

data Viz = Viz [Pic AVal]
  deriving Show
initViz :: Viz
initViz = Viz []

-- Match old and new Pics via id; new ones are just initialized via const
updateViz :: Float -> Viz -> [Pic AVal] -> Viz
updateViz t (Viz oldPics) newPics =
  let oldAndNew = filter hasNew $ pairUp oldPics newPics getTag getTag
   in Viz $ map merge oldAndNew
  where hasNew (_, Nothing) = False
        hasNew _ = True
        merge (Just oldPic, Just newPic) = updatePic t (t+duration) oldPic newPic
        merge (Nothing, Just newPic) = newPic

unR2 (V2 x y) = (x, y)

-- TODO: This matrix ioref in the state is a crime against nature
renderViz :: Float -> State -> Viz -> IO Picture
renderViz t s (Viz pics) = do
  mat <- readIORef (currentHypercubeMat s)
  let anims = map renderPic (map (mapPic (aValToId t)) pics)
      (hc, mat') = renderHypercube s mat t
  writeIORef (currentHypercubeMat s) mat'
  cursor <- sequenceCursor s
  --msp ("renderViz", cursor)
  return $ Pictures $ [cursor] ++ anims ++ [hc]

renderHypercube :: State -> Mat -> Float -> (Picture, Mat)
renderHypercube s mat t =
  let (hyp, mat') = transformHypercube s mat t
      picture = renderPolytope (showIt hyp)
   in (picture, mat')

-- Transform the current vertex of interest, from its current location, to the
-- camera-facing point (0, 0, -1, whatever...). Return the updated current matrix.
transformHypercube :: State -> Mat -> Float -> (Polytope, Mat)
transformHypercube s mat t = {-eesp debug $-} (applyMatrix mat' makeHypercube, mat')
  where srcOrig = {-eeesp "voi" $-} vertexOfInterest s
        src = mat !* srcOrig
        dest = pointingAtCamera
        dt = 0.1
        rot = rotateTowards dt src dest
        mat' = mat !*! rot
        debug = ("TH", srcOrig, src, dest, src `dot` dest, tSrc, tSrc `dot` dest, pt)
          where tSrc :: Pt
                tSrc = mat' !* srcOrig
                pt :: V2 Double
                pt = projectPt (moveAway + tSrc)

vertexOfInterest :: State -> Pt
-- hack!
vertexOfInterest (State { currentGroup }) | currentGroup == [] = pointingAtCamera
vertexOfInterest (State { currentGroup }) | otherwise =
  let s = take numDims (getHash (head currentGroup))
      coords = map (\x -> if x <= '7' then (-0.5) else 0.5) s
   in fromJust $ fromVector (V.fromList coords) :: Pt

--   where m = rotateTowards ang src dest
--         src = (getVerts makeHypercube) V.! 0
--         dest = (getVerts makeHypercube) V.! 1
--         ang = realToFrac t * (pi/4)

----transformHypercube t = rotatePolytope (ang/4) 1 3 (rotatePolytope (ang/2) 0 2 (rotatePolytope ang 0 1 makeHypercube))
--transformHypercube t = rotatePolytope (ang/4) 5 6 makeHypercube
--  where ang :: Double
--        ang = realToFrac t * (pi/4)

scaleByDim :: M.Map Int Double
scaleByDim = M.fromList [(3, 1000), (4, 6000), (8, 2500000)]

square :: [V2 Double]
square = [V2 o o, V2 no o, V2 no no, V2 o no]
  where o = 1
        no = (-1)

renderPolytope :: Polytope -> Picture
renderPolytope p =
  let proj :: [(V2 Double, V2 Double)]
      proj = projectPolytope p
      toLine (a, b) = Line [toPoint (trans a), toPoint (trans b)]
      toPoint :: V2 Double -> (Float, Float)
      toPoint (V2 x y) = (realToFrac x, realToFrac y)
      trans :: V2 Double -> V2 Double
      trans v = orig + scale *^ v
        where orig :: V2 Double
              orig = V2 (w/4) (-(h/4))
              V2 w h = fmap fromIntegral windowDim
              scale :: Double
              --scale = 2500000 -- 8d
              --scale = 1000 -- 3d
              scale = scaleByDim M.! numDims
      centerBox = Translate (w/4) (-(h/4)) $ rectangleWire 10 10
        where V2 w h = fmap fromIntegral windowDim
      -- centerBox = map Line edges
      --   where edges = toArr $ zip tsq (take 4 (drop 1 (cycle tsq)))
      --         toArr (a, b) = [a, b]
      --         tsq = map trans $ (map (^* 10)) square
   in Color black $ Pictures $ map toLine proj ++ [centerBox]

sequenceCursor :: State -> IO Picture
sequenceCursor s@(State { looper }) = do
  progress <- getProgress looper
  --msp ("mp", progress)
  --let progress = 0.3
  --msp $ ("cs", case s of State { currentSong } -> currentSong)
  return $ renderProgress s progress

loopColor :: Loop -> Color
loopColor loop =
  let hash = getHash loop
      ri = read ("0x" ++ (take 2 hash)) :: Int
      gi = read ("0x" ++ (take 2 $ drop 2 hash)) :: Int
      bi = read ("0x" ++ (take 2 $ drop 4 hash)) :: Int
      r = fromIntegral ri / 256.0
      g = fromIntegral gi / 256.0
      b = fromIntegral bi / 256.0
   in makeColor r g b 1.0

rectWidth :: Float
rectWidth = 25
rectHeight :: Float
rectHeight = 20
rectDim :: V2 Float
rectDim = V2 rectWidth rectHeight
rectThickness :: Float
rectThickness = 3

markMargin = 10
markThickness = 3
markDim = rectDim + (V2 markMargin markMargin)

markRect :: Picture
markRect = Color black $ thickBorder markThickness (V2 0 0) markDim
-- markRect = Color black $ rectangleWire w h
--   where V2 w h = rectDim + 2 * m
--         V2 tx ty = (-m)
--         m = V2 markMargin markMargin

rect :: Color -> Color -> Picture
rect color borderColor = Pictures [bg, border]
  where bg = Color color $ Polygon $ rectanglePath rectWidth rectHeight
        border = rectBorder borderColor
        --border = Color black $ lineLoop $ rectanglePath 25.0 20.0
        --waveForm = Color black $ Line [(-10.0, -10.0), (10.0, 10.0)]

rectBorder :: Color -> Picture
--rectBorder color = Color black $ lineLoop $ rectanglePath 25.0 20.0
rectBorder color = Color color $ thickBorder rectThickness (V2 0 0) rectDim

rectAt :: V2 Float -> V2 Float -> Picture
rectAt a b = Translate tx ty $ Polygon $ rectanglePath w h
  where V2 w h = b - a
        V2 tx ty = ((b - a) / 2) + a

thickLine :: Float -> V2 Float -> V2 Float -> Picture
thickLine thickness a b = Polygon (map unR2 pts)
  where along = signorm (b - a) ^* (thickness/2)
        left = perp along
        pts = [a - along - left,
               b + along - left,
               b + along + left,
               a - along + left]

thickBorder :: Float -> V2 Float -> V2 Float -> Picture
thickBorder thickness a b = Pictures lines
  where lines = map (uncurry (thickLine thickness)) (pairUp pts)
        pairUp pts = zip pts (drop 1 (cycle pts))
        pts = map center [a, ab, b, ba]
        center v = v - ((b - a) / 2)
        ab = blah b a
        ba = blah a b
        blah (V2 x _) (V2 _ y) = V2 x y

upTri :: Picture
upTri = Color black $ Scale scale scale $ Polygon pts
  where pts = [(0.0, 0.5), (0.5, (-0.5)), ((-0.5), (-0.5))]
        scale = 10

renderPic :: Pic Id -> Picture
renderPic (LoopP (LoopT loop) (Id (V2 x y))) = Translate x y $ rect color black
  where color = loopColor loop
renderPic (SeqP (SeqT loop _ _) (Id (V2 x y)) (Id size)) = Translate x y $ rect color black
  where color = loopColor loop
renderPic (LoopPlaceP (LoopPlaceT loop) (Id (V2 x y))) = Translate x y $ rect color borderColor
  where color = withAlpha a $ loopColor loop
        borderColor = withAlpha a $ black
        a = 0.2
renderPic (MarkP (MarkT i) (Id (V2 x y))) = Translate x y $ markRect

stateToPics :: Float -> State -> State -> [Pic AVal]
stateToPics t oldS s = loopPlacePics s ++ affinitiesToPics s ++ sequenceToPics t oldS s

loopPlacePics :: State -> [Pic AVal]
loopPlacePics s@(State { loops }) = map toPic loops
  where toPic loop = constPic $ LoopPlaceP (LoopPlaceT loop) (Id pos)
          where pos = (gridPosition loop s)

affinitiesToPics :: State -> [Pic AVal]
affinitiesToPics s@(State { loops }) = map toPic loops ++ marks
  where toPic loop = constPic $ LoopP (LoopT loop) (Id pos)
          where pos = case aps M.!? loop of Just pos -> pos
                                            Nothing -> (gridPosition loop s)
                aps = affinityPositions s
        marks = map (uncurry toMark) (zip [0..] (currentGroup s))
        toMark i loop = constPic $ MarkP (MarkT i) (Id (gridPosition loop s))

sequenceToPics :: Float -> State -> State -> [Pic AVal]
sequenceToPics t _ (State { currentSong = Nothing }) = []
sequenceToPics t oldS s =
  let State { currentSong = Just (score, loops) } = s
      theSame = currentSong oldS == currentSong s
   in L.zipWith (toPic theSame) [0..] (endPositions score loops)
  where toPic theSame i (loop, pos) = if (esp theSame) then constPic endPic else combine startPic endPic
          where endPic = SeqP (SeqT loop i t) (Id pos) (Id 10.0)
                startPic = SeqP (SeqT loop i t) (Id startPos) (Id 10.0)
                startPos = aps M.! loop
        combine p p' = updatePic (t+2*duration) (t+3*duration) (constPic p) (constPic p')
        endPositions score loops = (seqLayOutPositions $ seqLoopsAndPositions score loops)
        aps = affinityPositions s

seqLayOutPositions :: [(Loop, V2 Int)] -> [(Loop, V2 Float)]
seqLayOutPositions poses = map lop poses
  where lop (loop, pos) = (loop, fpos pos)
        fpos pos = fmap fromIntegral pos * room - (window / 2.0) + seqMargin
        window = fmap fromIntegral $ V2 windowWidth windowHeight
        room :: V2 Float
        room = V2 32.0 27.0
        allXs :: [Int]
        allXs = map (\(_, V2 x _) -> x) poses
        allYs :: [Int]
        allYs = map (\(_, V2 _ y) -> y) poses
        seqSizeI :: V2 Int
        seqSizeI = V2 (maximum allXs) (maximum allYs)
        seqSize :: V2 Float
        seqSize = fmap fromIntegral seqSizeI * room
        seqMargin :: V2 Float
        seqMargin = (seqWindow - seqSize) / 2
        seqWindow :: V2 Float
        seqWindow = window / 2 -- V2 (windowWidth `div` 2) (windowHeight `div` 2)

-- TODO really shouldn't duplicate this, but how?
renderProgress :: State -> Float -> Picture
renderProgress (State { currentSong = Just (score, loops) }) progress = Translate tx (ty - 5) $ upTri
  where tx = interp progress 0 1 left right
        V2 _ ty = (-(window / 2)) + seqMargin - room / 2
        V2 left _ = (-(window / 2)) + seqMargin - room / 2
        V2 right _ = (-seqMargin) + room / 2
        window = fmap fromIntegral $ V2 windowWidth windowHeight
        room :: V2 Float
        room = V2 32.0 27.0
        allXs :: [Int]
        allXs = map (\(_, V2 x _) -> x) poses
        allYs :: [Int]
        allYs = map (\(_, V2 _ y) -> y) poses
        seqSizeI :: V2 Int
        seqSizeI = V2 (maximum allXs) (maximum allYs)
        seqSize :: V2 Float
        seqSize = fmap fromIntegral seqSizeI * room
        seqMargin :: V2 Float
        seqMargin = (seqWindow - seqSize) / 2
        seqWindow :: V2 Float
        seqWindow = window / 2 -- V2 (windowWidth `div` 2) (windowHeight `div` 2)
        poses = seqLoopsAndPositions score loops
renderProgress _ _ = Blank

seqLoopsAndPositions :: Score -> [[Loop]] -> [(Loop, V2 Int)]
seqLoopsAndPositions (Score measureses) loops = concat $ zipWith col measureses [0..]
  where col :: [Measure] -> Int -> [(Loop, V2 Int)]
        col measures x = zipWith (one x) measures [0..]
        one :: Int -> Measure -> Int -> (Loop, V2 Int)
        one x (Measure (g, i) _) y = ((loops !! g) !! i, V2 x y)

reportViz :: Viz -> Viz
reportViz = id
--reportViz v = eesp (gcReport v) v

stateToViz :: State -> Viz -> State -> Float -> Viz
stateToViz oldS v s t = reportViz $ updateViz t v (stateToPics t oldS s)

gridPosition :: Loop -> State -> V2 Float
gridPosition loop (State { loops }) =
  let i = fromJust $ loop `L.elemIndex` loops
      xflip (V2 x y) = V2 (-x) y
      window = V2 windowWidth windowHeight
      subWindow = (fmap fromIntegral window) / 2.0 - margin * 2
      margin = pure $ fromIntegral $ (min windowWidth windowHeight) `div` 20
   in xflip $ subWindow * toGridXYF i (length loops) + margin
