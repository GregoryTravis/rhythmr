-- Code lifted from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

module RPC (rpc) where

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)

import ClientServer
import Util

rpc = do
  forkIO $ do
    threadDelay $ 1 * 1000000
    client
  threadId <- forkIO $ server
  msp "running"
  threadDelay $ 5 * 1000000
  killThread threadId
