module Display
( DisplayRequest(..)
, DispayResponse(..)
, displayServer
, withDisplay
, displaySend ) where

import Control.Concurrent (forkIO, threadDelay)
import Graphics.Gloss hiding (Display)

import RPC
import Util

data Display = Display RPCClient

data DisplayRequest = Circ Float deriving (Read, Show)
data DispayResponse = DispayResponse deriving (Read, Show)

displayServer = do
  forkIO $ do
    rpcServer 3000 serverHandler
    threadDelay $ 7200 * 1000000
  return ()

serverHandler :: DisplayRequest -> IO DispayResponse

serverHandler (Circ w) = do
  display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle w)
  return DispayResponse

withDisplay :: (Display -> IO a) -> IO a
withDisplay handler = withRPCClient "127.0.0.1" 3000 $ \c -> handler $ Display c

displaySend :: Display -> DisplayRequest -> IO DispayResponse
displaySend (Display c) request = rpcSend c request
