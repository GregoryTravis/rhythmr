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
  , updateFiz
  ) where

import Data.IORef
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Maybe (fromJust, fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Juicy as J
import Linear
import Linear.V (fromVector)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

import Animate
import Constants
import Fiz
import Gui
import qualified Hash as H
import Hypercube
import Loop
import Looper
import Memoize
import State
import Util
import Zounds hiding (Translate, Scale)

instance Ord Color where
  compare c c' = compare (rgbaOfColor c) (rgbaOfColor c')

-- demo mode
duration = 1.5
-- duration = 0.5

logo :: Picture
logo = fromJust $ unsafePerformIO $ J.loadJuicyPNG "i/64.png"
logoName :: Picture
logoName = fromJust $ unsafePerformIO $ J.loadJuicyPNG "i/logo-name.png"

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
affinityPositions s = case affinities s of xss -> M.fromList $ concat (zipWith rah (gridTransformsForN (length xss)) xss)
  where rah :: (V2 Float -> V2 Float) -> [Loop] -> [(Loop, V2 Float)]
        rah xform xs = zip xs $ map (\cXform -> ((scaler (V2 400 400)) . xform . cXform) (V2 0 0)) (ringOfCirclesInUnitSquare (length xs))

-- Draw the current group in the middle of the screen
currentPositions :: State -> M.Map Loop (V2 Float)
currentPositions (State { currentGroup }) = M.fromList (zip currentGroup (map (place . ($ V2 0 0)) (ringOfCirclesInUnitSquare (length currentGroup))))
  where place :: V2 Float -> V2 Float
        place = (translater (fmap fromIntegral (V2 0 (- (windowHeight `div` 4))))) . (scaler (V2 500 500)) . (subtract (V2 0.5 0.5))
--currentPositions (State { currentGroup }) = M.fromList (zip currentGroup (repeat (V2 0 0)))

interpV :: Float -> Float -> Float -> V2 Float -> V2 Float -> V2 Float
interpV t s e (V2 x y) (V2 x' y') = V2 x'' y''
  where x'' = interp t s e x x'
        y'' = interp t s e y y'

interp :: Float -> Float -> Float -> Float -> Float -> Float
--interp t s e a a' = eesp (t, s, e, a, a') $ a + (k * (a' - a))
interp t s e a a' = a + (k * (a' - a))
  where k = clip 0.0 1.0 ((t - s) / (e - s))

-- I know I know
interpD :: Double -> Double -> Double -> Double -> Double -> Double
--interp t s e a a' = eesp (t, s, e, a, a') $ a + (k * (a' - a))
interpD t s e a a' = a + (k * (a' - a))
  where k = clip 0.0 1.0 ((t - s) / (e - s))

clip :: (Ord a, Num a) => a -> a -> a -> a
clip lo hi x | x < lo = lo
clip lo hi x | x > hi = hi
clip lo hi x | otherwise = x

-- Some potential for inconsistency here, Pic and its Tag could differ
data Tag = LoopT Loop | SeqT Loop Int Float | LoopPlaceT Loop | MarkT Int | CurT Loop
  deriving (Eq, Show, Ord)
data Pic c = LoopP Tag (c (V2 Float)) Color
           | SeqP Tag (c (V2 Float)) (c Float) Color
           | LoopPlaceP Tag (c (V2 Float)) Color
           | MarkP Tag (c (V2 Float))
           | CurP Tag (c (V2 Float)) Color
deriving instance () => Show (Pic AVal)
deriving instance () => Show (Pic Id)

data Id a = Id a
  deriving Show

mapPic :: (forall a . (Eq a, Show a) => c a -> c' a) -> Pic c -> Pic c'
mapPic f (SeqP tag pos width color) = SeqP tag (f pos) (f width) color
mapPic f (LoopP tag pos color) = LoopP tag (f pos) color
mapPic f (LoopPlaceP tag pos color) = LoopPlaceP tag (f pos) color
mapPic f (MarkP tag pos) = MarkP tag (f pos)
mapPic f (CurP tag pos color) = CurP tag (f pos) color

zipWithPic :: (forall a . (Eq a, Show a) => c a -> d a -> e a) -> Pic c -> Pic d -> Pic e
zipWithPic f (SeqP tag pos width color) (SeqP tag' pos' width' color') | tag == tag' && color == color' = SeqP tag (f pos pos') (f width width') color
zipWithPic f (LoopP tag pos color) (LoopP tag' pos' color') | tag == tag' && color == color' = LoopP tag (f pos pos') color
zipWithPic f (LoopPlaceP tag pos color) (LoopPlaceP tag' pos' color') | tag == tag' && color == color' = LoopPlaceP tag (f pos pos') color
zipWithPic f (MarkP tag pos) (MarkP tag' pos') | tag == tag' = MarkP tag (f pos pos')
zipWithPic f (CurP tag pos color) (CurP tag' pos' color') | tag == tag' && color == color' = CurP tag (f pos pos') color

mapSquish :: (forall a . (Eq a, Show a) => c a -> b) -> Pic c -> [b]
mapSquish f (SeqP tag pos width _) = [f pos, f width]
mapSquish f (LoopP tag pos _) = [f pos]
mapSquish f (LoopPlaceP tag pos _) = [f pos]
mapSquish f (MarkP tag pos) = [f pos]
mapSquish f (CurP tag pos _) = [f pos]

gcReport :: Viz -> [Int]
gcReport (Viz pics _) = concat $ map (mapSquish aValSize) pics

getTag :: Pic a -> Tag
getTag (LoopP tag _ _) = tag
getTag (SeqP tag _ _ _) = tag
getTag (LoopPlaceP tag _ _) = tag
getTag (MarkP tag _) = tag
getTag (CurP tag _ _) = tag

picInterpolator :: Pic c -> Pic Interpolator
picInterpolator (LoopP tag _ color) =
  LoopP tag v2FloatInterpolator color
picInterpolator (SeqP tag _ _ color) =
  SeqP tag v2FloatInterpolator floatInterpolator color
picInterpolator (LoopPlaceP tag _ color) =
  LoopPlaceP tag v2FloatInterpolator color
picInterpolator (MarkP tag _) =
  MarkP tag v2FloatInterpolator
picInterpolator (CurP tag _ color) =
  CurP tag v2FloatInterpolator color

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

data Viz = Viz [Pic AVal] (Fiz Loop)
  deriving Show
initViz :: Viz
initViz = Viz [] emptyFiz

-- Match old and new Pics via id; new ones are just initialized via const
updateViz :: Float -> Viz -> [Pic AVal] -> Viz
updateViz t (Viz oldPics fiz) newPics =
  let oldAndNew = filter hasNew $ pairUp oldPics newPics getTag getTag
   in Viz (map merge oldAndNew) fiz
  where hasNew (_, Nothing) = False
        hasNew _ = True
        merge (Just oldPic, Just newPic) = updatePic t (t+duration) oldPic newPic
        merge (Nothing, Just newPic) = newPic

unR2 (V2 x y) = (x, y)

-- TODO: This matrix ioref in the state is a crime against nature
renderViz :: Float -> State -> Viz -> IO Picture
renderViz t s (Viz pics fiz) = do
  mat <- readIORef (currentHypercubeMat s)
  let anims = map renderPic (map (mapPic (aValToId t)) pics)
      (hc, mat') = renderHypercube s mat t
      strategy = renderStrategy s
      labels = renderLabels
  writeIORef (currentHypercubeMat s) mat'
  progress <- getProgress (looper s)
  -- (tx, cursor) <- sequenceCursor s
  let seqPics = map (Translate (-progress) 0) $ map renderPic $ map (mapPic (aValToId t)) $ renderCurrentSong progress s
  --msp ("renderViz", cursor)
  let fizMaybe = if (useFiz s) then renderFiz s fiz else []
      animsMaybe = if (useFiz s) then [] else anims
  let ph = playHeadMaybe s
      w = (fromIntegral $ windowWidth `div` 2) - margin - 0
      h = (-((fromIntegral $ windowHeight `div` 2) - margin)) + (-4)
      margin = 32 + 16
      logo' = Translate w h logo
      logoName' = Translate (w - 98) h $ Scale 0.3 0.3 logoName
  return $ Pictures $ [hc] ++ seqPics ++ ph ++ [logo', logoName'] ++ animsMaybe ++ [strategy] ++ labels ++ fizMaybe

renderFiz :: State -> Fiz Loop -> [Picture]
renderFiz s fiz = map toPic unliked ++ (fizEdges s fiz) ++ map toPic liked
  where likedSet = S.unions $ map S.fromList (likes s)
        unlikedSet = (S.fromList (loops s)) `S.difference` likedSet
        liked = S.toList likedSet
        unliked = S.toList unlikedSet
        toPic :: Loop -> Picture
        toPic loop = Translate x y $ rect (colorFor loop) black
          where V2 x y = getPos fiz loop
        allLiked = S.unions (map S.fromList (likes s))
        colorFor loop | S.member loop allLiked = loopColor loop
                      | otherwise = alphaLoopColor loop

fizEdges :: State -> Fiz Loop -> [Picture]
fizEdges s fiz = map (groupEdges fiz) (likes s)

groupEdges :: Fiz Loop -> [Loop] -> Picture
groupEdges fiz loops = Color color $ Polygon ptsClosed
  where ptsClosed = take (length pts + 1) (cycle pts)
        pts = map toPoint (map (getPos fiz) loops)
        toPoint (V2 x y) = (x, y)
        color = withAlpha 0.15 $ mixColors' (map loopColor loops)

mixColors' :: [Color] -> Color
mixColors' [] = error "mixColors: empty"
mixColors' colors =
  let tuples = map rgbaOfColor colors
      vs = map (\(r, g, b, a) -> V4 r g b a) tuples
      mixedV = sum vs / (fromIntegral $ length vs)
   in case mixedV of V4 r g b a -> makeColor r g b a

renderLabels :: [Picture]
renderLabels = [ at (-335) (350) "Loops"
               , at 30 (350) "Affinities"
               , at (-60) (-60) "Current stack" ]
  where at x y s = Translate x y $ Scale 0.15 0.15 $ Text s

renderStrategy (State { strategy = Nothing }) = Blank
renderStrategy (State { strategy = Just strategy }) =
  let V2 tx ty = fmap fromIntegral $ (fmap (`div` 2) $ V2 (-windowWidth) (-windowHeight)) + margin
      margin = V2 30 (45)
      s = 0.25
   in Translate tx ty $ Scale s s $ Text strategy

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
        rot = rotateTowards (pi/256) src dest
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
scaleByDim = M.fromList [(3, 1000), (4, 4000), (8, 2500000)]

square :: [V2 Double]
square = [V2 o o, V2 no o, V2 no no, V2 o no]
  where o = 1
        no = (-1)

renderPolytope :: Polytope -> Picture
renderPolytope p =
  let proj :: [(DepthPt, DepthPt)]
      proj = projectPolytope p
      --toLine (DepthPt a ad, DepthPt b bd) = Color (toColor ((ad+bd)/2)) $ Line [toPoint (trans a), toPoint (trans b)]
      toLine (DepthPt a ad, DepthPt b bd) = fadeLine 4 (trans a) (trans b) ad bd nearColor farColor
      toPoint :: V2 Double -> (Float, Float)
      toPoint (V2 x y) = (realToFrac x, realToFrac y)
      toColor :: Double -> Color
      toColor d = mixColors (realToFrac (1 - d)) (realToFrac d) nearColor farColor
      trans :: V2 Double -> V2 Double
      trans v = orig + scale *^ v
        where orig :: V2 Double
              orig = V2 0 0
        -- where orig :: V2 Double
        --       orig = V2 (w/4) (-(h/4))
        --       V2 w h = fmap fromIntegral windowDim
              scale :: Double
              --scale = 2500000 -- 8d
              --scale = 1000 -- 3d
              scale = (scaleByDim M.! numDims) * 2.5
      centerBox = Translate (w/4) (-(h/4)) $ rectangleWire 10 10
        where V2 w h = fmap fromIntegral windowDim
      nearColor = makeColor grey grey grey 1
      farColor = makeColor grey grey grey 0
      grey = 0.8
      -- centerBox = map Line edges
      --   where edges = toArr $ zip tsq (take 4 (drop 1 (cycle tsq)))
      --         toArr (a, b) = [a, b]
      --         tsq = map trans $ (map (^* 10)) square
   in Pictures $ map toLine proj ++ [centerBox]

fadeLine :: Int -> V2 Double -> V2 Double -> Double -> Double -> Color -> Color -> Picture
fadeLine n a b t0 t1 c0 c1 = Pictures $ zipWith cl colors pairs
  where ts :: [Double]
        ts = map (/fromIntegral n) (map fromIntegral [0..n])
        tts :: [Double]
        tts = map (\t -> interpD t 0 1 t0 t1) ts
        pts :: [V2 Double]
        pts = map (\t -> lerp t a b) ts
        pairs :: [(V2 Double, V2 Double)]
        pairs = zip pts (tail (cycle pts))
        cl :: Color -> (V2 Double, V2 Double) -> Picture
        cl color (V2 x y, V2 x' y') = Color color $ Line [(realToFrac x, realToFrac y), (realToFrac x', realToFrac y')]
        colors :: [Color]
        colors = map toColor tts
        toColor :: Double -> Color
        toColor t = mixColors (realToFrac (1 - t)) (realToFrac t) c0 c1
        debug = (n, t0, t1, ts, tts)

--sequenceCursor :: State -> IO (Float, Picture)
--sequenceCursor s@(State { looper }) = do
--  progress <- getProgress looper
--  --msp ("mp", progress)
--  --let progress = 0.3
--  --msp $ ("cs", case s of State { currentSong } -> currentSong)
--  return $ renderProgress s progress

loopColor' :: Loop -> Color
loopColor' = hashColor . getHash
loopColor = unsafePerformIO (memoizePure loopColor')

hashColor' :: String -> Color
hashColor' hash =
  let ri = read ("0x" ++ (take 2 hash)) :: Int
      gi = read ("0x" ++ (take 2 $ drop 2 hash)) :: Int
      bi = read ("0x" ++ (take 2 $ drop 4 hash)) :: Int
      r = fromIntegral ri / 256.0
      g = fromIntegral gi / 256.0
      b = fromIntegral bi / 256.0
   in makeColor r g b 1.0
hashColor = unsafePerformIO (memoizePure hashColor')

stringColor :: String -> Color
stringColor = hashColor . H.hash

alphaLoopColor' loop = withAlpha 0.2 (loopColor' loop)
alphaLoopColor = unsafePerformIO (memoizePure alphaLoopColor')

borderColor = withAlpha 0.2 $ black

rectWidth :: Float
rectWidth = 25
rectHeight :: Float
rectHeight = 20
rectDim :: V2 Float
rectDim = V2 rectWidth rectHeight
rectThickness :: Float
rectThickness = 1

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
rect = vRect rectWidth

vRect :: Float -> Color -> Color -> Picture
vRect width color borderColor = Pictures [bg, border]
  where bg = Color color $ Polygon $ rectanglePath width rectHeight
        border = vRectBorder width borderColor
        --border = Color black $ lineLoop $ rectanglePath 25.0 20.0
        --waveForm = Color black $ Line [(-10.0, -10.0), (10.0, 10.0)]

vRectBorder :: Float -> Color -> Picture
vRectBorder width color = Color color $ thickBorder rectThickness (V2 0 0) (V2 width rectHeight)

--rectBorder :: Color -> Picture
----rectBorder color = Color black $ lineLoop $ rectanglePath 25.0 20.0
--rectBorder color = Color color $ thickBorder rectThickness (V2 0 0) rectDim

--rectAt :: V2 Float -> V2 Float -> Picture
--rectAt a b = Translate tx ty $ Polygon $ rectanglePath w h
--  where V2 w h = b - a
--        V2 tx ty = ((b - a) / 2) + a

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

downTri :: Picture
downTri = Scale (-1) (-1) upTri

renderPic :: Pic Id -> Picture
renderPic (LoopP (LoopT loop) (Id (V2 x y)) color) = Translate x y $ rect color black
renderPic (SeqP (SeqT loop _ _) (Id (V2 x y)) (Id width) color) = Translate x y $ vRect width color black
renderPic (LoopPlaceP (LoopPlaceT loop) (Id (V2 x y)) color) = Translate x y $ rect color borderColor
renderPic (MarkP (MarkT i) (Id (V2 x y))) = Translate x y $ markRect
renderPic (CurP (CurT loop) (Id (V2 x y)) color) = Translate x y $ rect color black

stateToPics :: Float -> State -> State -> [Pic AVal]
stateToPics t oldS s = loopPlacePics s ++ affinitiesToPics s ++ currentsToPics s -- ++ renderCurrentSong t s

loopPlacePics :: State -> [Pic AVal]
loopPlacePics s@(State { loops }) = map toPic loops
  where toPic loop = constPic $ LoopPlaceP (LoopPlaceT loop) (Id pos) (alphaLoopColor loop)
          where pos = (gridPosition loop s)

affinitiesToPics :: State -> [Pic AVal]
affinitiesToPics s@(State { loops }) = map toPic loops ++ marks
  where toPic loop = constPic $ LoopP (LoopT loop) (Id pos) (loopColor loop)
          where pos = fromJust $ applyMaybes [(aps M.!?), (curs M.!?), const (Just (gridPosition loop s))] loop
                aps = affinityPositions s
                curs = currentPositions s
        marks = map (uncurry toMark) (zip [0..] (currentGroup s))
        toMark i loop = constPic $ MarkP (MarkT i) (Id (gridPosition loop s))

currentsToPics :: State -> [Pic AVal]
currentsToPics s@(State { loops }) = map toPic loops
  where toPic loop = constPic $ CurP (CurT loop) (Id pos) (loopColor loop)
          where pos = fromJust $ applyMaybes [(curs M.!?), (aps M.!?), const (Just (gridPosition loop s))] loop
                aps = affinityPositions s
                curs = currentPositions s

playHeadMaybe :: State -> [Picture]
playHeadMaybe (State { currentSong = Nothing }) = []
playHeadMaybe _ = [playHead]
  where playHead = Translate 0 ht $ downTri
        ht = (-(fromIntegral windowHeight / 8)) + 25

renderCurrentSong :: Float -> State -> [Pic AVal]
renderCurrentSong progress (State { currentSong = Nothing }) = []
renderCurrentSong progress (State { currentSong = Just (z, renderedZ) }) =
  let songBounds = getBounds z
      toPic :: Int -> Zound -> Bounds -> Pic AVal
      toPic row z b = constPic (SeqP (SeqT (Loop (filenameOf z)) 0 0.0) (Id $ segmentPos row b) (Id $ segmentWidth b) (colorFor z))
      filenameOf :: Zound -> String
      filenameOf z = case source z of Just (Source [filename]) -> filename
      colorFor :: Zound -> Color
      colorFor z = stringColor (filenameOf z)
      segmentPos :: Int -> Bounds -> V2 Float
      segmentPos row (Bounds s e) = V2 (toScreen (s + ((e - s) `div` 2))) (rowOffset - (fromIntegral windowHeight / 8))
        where rowOffset = (-(fromIntegral row * (rectWidth + 5)))
      segmentWidth :: Bounds -> Float
      segmentWidth (Bounds s e) = (toScreen e - toScreen s) - 4
      -- Convert sample num to screen space
      toScreen :: Frame -> Float
      toScreen frame = ((fromIntegral (frame - songTimeFrames)) / (fromIntegral loopLengthFrames)) * (rectWidth + seqMargin) * stretch
      songTimeFrames :: Frame
      songTimeFrames = floor $ progress * (fromIntegral (numFrames renderedZ))
      --tFrames = timeToFrame t
      seqMargin = 5
      allBounds = getAllBounds z
      allSegments = getAllSegments z
      boundsToSegment :: M.Map Bounds Zound
      boundsToSegment = M.fromList (zip allBounds allSegments)
      ok = (length allBounds) == (length allSegments)
      stretch = 10.0
      stackedBounds = stackBounds (zip allBounds allSegments)
      picses :: [Pic AVal]
      picses = concat $ zipWith (\row bs -> map (\(bounds, z) -> toPic row z bounds) bs) [0..] stackedBounds
   in {-fesp (take 10 . map jeh) $ eesp ("huh", take 10 allBounds) $-} assertM "bounds/segments mismatch" ok $ picses
  where jeh (SeqP _ pos wid _) = (pos, wid)

-- Distribute entities on mutliple rows so as to minimize overlap, up to a
-- maximum number of rows.
-- - sort by left edge
-- - Create empty rows
-- - for each segment
--   - if it is past the end of any row, add it to the row that has the greatest rightward extent
--   - if it is not, add it to the row that has the least rightward extent
stackBounds :: [(Bounds, Zound)] -> [[(Bounds, Zound)]]
stackBounds bs = addBounds (take numRows $ repeat []) (sortBounds bs)
  where addBounds :: [[(Bounds, Zound)]] -> [(Bounds, Zound)] -> [[(Bounds, Zound)]]
        addBounds stack [] = stack
        addBounds stack (b:bs) = addBounds (addBound stack b) bs
        addBound :: [[(Bounds, Zound)]] -> (Bounds, Zound) -> [[(Bounds, Zound)]]
        addBound stack b = overBest (++ [b]) (closest b) stack
          where fep = ("fep", fst b, map (closest b) stack, map gep stack)
                gep [] = -100
                gep xs = getEnd $ fst $ last xs
        -- Compare the start of the bounds we wish to place with the ends of all the rows
        -- e to the left of s is always better, and in this case the higest e wins
        -- otherwise, the lowest e wins.
        -- If a row is empty, treat the 'end' of its 'bounds' as 0
        -- Returns (side, score), where side is 1 if e < s
        -- By "to the left" we mean <, but because of round errors mean 'within [fudge factor]
        -- of being to the left'.
        closest :: (Bounds, Zound) -> [(Bounds, Zound)] -> (Int, Frame)
        closest b bs = (side, compareSE s e)
          where side | e <= s + fudge = 1
                     | otherwise = 0
                s = getStart (fst b)
                e | null bs = 0
                  | otherwise = getEnd (fst (last bs))
                compareSE s e = (-(abs (s - e)))
                fudge = 3
                -- compareSE s e | e <= s = e - s
                --               | otherwise = e - s
        -- -- Since we sorted the list, we can just check the last element of the
        -- -- list; if the list is empty, we treat the rightmost extent of that
        -- -- row as 0.
        -- closest b [] = (-(abs (getStart (fst b) - 0)))
        -- closest b bs = (-(abs (getStart (fst b) - getEnd (fst (last bs)))))
        numRows = 8
        sortBounds = L.sortOn (getStart . fst)

-- Score each element of the list, and call the function on the one with the
-- highest score, replacing it.
overBest :: (Ord n) => (a -> a) -> (a -> n) -> [a] -> [a]
overBest _ _ [] = error "overBest: empty list"
overBest f scorer xs = replaceInList xs bestIndex (f (xs !! bestIndex))
  where bestIndex = fromMaybe err (L.elemIndex bestScore scores)
        err = error "overBest: ???"
        scores = map scorer xs
        bestScore = maximum scores

{-
sequenceToPics :: Float -> State -> [Pic AVal]
sequenceToPics t (State { currentSong = Nothing }) = []
sequenceToPics t s =
  let State { currentSong = Just loops } = s
   in L.zipWith (toPic True) [0..] (endPositions loops)
  where toPic theSame i (loop, pos) = if (esp theSame) then constPic endPic else combine startPic endPic
          where endPic = SeqP (SeqT loop i t) (Id pos) (Id 10.0) color
                startPic = SeqP (SeqT loop i t) (Id startPos) (Id 10.0) color
                startPos = aps M.! loop
                color = loopColor loop
        combine p p' = updatePic (t+2*duration) (t+3*duration) (constPic p) (constPic p')
        endPositions loops = (seqLayOutPositions $ seqLoopsAndPositions loops)
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
        seqWindow = window / V2 1.0 2.0 -- V2 (windowWidth `div` 2) (windowHeight `div` 2)

-- -- TODO really shouldn't duplicate this, but how?
-- renderProgress :: State -> Float -> (Float, Picture)
-- renderProgress (State { currentSong = Just loops }) progress = (tx, Translate 0 (ty - 5) $ upTri)
--   where tx = interp progress 0 1 left right
--         V2 _ ty = (-(window / 2)) + seqMargin - room / 2
--         V2 left _ = (-(window / 2)) + seqMargin - room / 2
--         V2 right _ = (window / 2) - seqMargin + room / 2
--         window = fmap fromIntegral $ V2 windowWidth windowHeight
--         room :: V2 Float
--         room = V2 32.0 27.0
--         allXs :: [Int]
--         allXs = map (\(_, V2 x _) -> x) poses
--         allYs :: [Int]
--         allYs = map (\(_, V2 _ y) -> y) poses
--         seqSizeI :: V2 Int
--         seqSizeI = V2 (maximum allXs) (maximum allYs)
--         seqSize :: V2 Float
--         seqSize = fmap fromIntegral seqSizeI * room
--         seqMargin :: V2 Float
--         seqMargin = (seqWindow - seqSize) / 2
--         seqWindow :: V2 Float
--         seqWindow = window / V2 1.0 2.0 -- V2 (windowWidth `div` 2) (windowHeight `div` 2)
--         poses = seqLoopsAndPositions loops
-- renderProgress _ _ = (0, Blank)

seqLoopsAndPositions :: [[Loop]] -> [(Loop, V2 Int)]
seqLoopsAndPositions loopses =
  [(loop, V2 x y) | (loops', x) <- zip loopses [0..], (loop, y) <- zip loops' [0..]]
-- seqLoopsAndPositions (Score measureses) loops = concat $ zipWith col measureses [0..]
--   where col :: [Measure] -> Int -> [(Loop, V2 Int)]
--         col measures x = zipWith (one x) measures [0..]
--         one :: Int -> Measure -> Int -> (Loop, V2 Int)
--         one x (Measure (g, i) _) y = ((loops !! g) !! i, V2 x y)
-}

reportViz :: Viz -> Viz
reportViz = id
--reportViz v = eesp (gcReport v) v

stateToViz :: State -> Viz -> State -> Float -> Viz
stateToViz oldS v s t = reportViz $ updateViz t v (stateToPics t oldS s)

updateFiz :: Float -> State -> Viz -> Viz
updateFiz dt s (Viz pics fiz) = Viz pics fiz'
  where fiz' = update dt (loops s) (likes s) fiz

gridPosition :: Loop -> State -> V2 Float
gridPosition loop (State { loops }) =
  let i = fromJust $ loop `L.elemIndex` loops
      xflip (V2 x y) = V2 (-x) y
      window = V2 windowWidth windowHeight
      subWindow = (fmap fromIntegral window) / 2.0 - margin * 2
      margin = pure $ fromIntegral $ (min windowWidth windowHeight) `div` 20
   in xflip $ subWindow * toGridXYF i (length loops) + margin
