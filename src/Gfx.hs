module Gfx
( gfxMain ) where

import Data.IORef
--This works if you uncomment gl in the cabal file
--import Graphics.GL (glBindTextureUnit)
import Graphics.UI.GLUT
import System.Exit

import Util

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

gfxMain :: IO ()
gfxMain = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= Size 800 800
  _window <- createWindow "Hello World"
  ioref <- newIORef 0
  displayCallback $= display ioref
  idleCallback $= (Just $ display ioref)
  keyboardMouseCallback $= Just keyboardMouseHandler
  mainLoop

keyboardMouseHandler :: KeyboardMouseCallback
keyboardMouseHandler (MouseButton LeftButton)_ _ _ = exitWith ExitSuccess
keyboardMouseHandler (Char 'q')              _ _ _ = exitWith ExitSuccess
keyboardMouseHandler _                       _ _ _ = postRedisplay Nothing

display :: IORef Float -> IO ()
display ioref = do
  t <- readIORef ioref
  msp ("draw", t)
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f 0.2 0.2 0
    vertex3f 0.2 0 0

    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f 0.2 (-0.2) 0
    vertex3f 0.2 0 0

    color3f 0 0 1
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f (-0.2) (-0.2) 0
    vertex3f (-0.2) 0 0

    color3f 1 0 1
    vertex3f 0 0 0
    vertex3f t 0.2 0
    vertex3f (-0.2) 0.2 0
    vertex3f (-0.2) 0 0
  flush
  writeIORef ioref (t + 0.02)
