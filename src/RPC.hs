-- Code lifted from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

module RPC (rpc) where

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)

import ClientServer
import Util

handler s = return $ s ++ ":" ++ s

rpc = do
  forkIO $ do
    threadDelay $ 1 * 1000000
    client "127.0.0.1" 3000
  threadId <- forkIO $ server 3000 handler
  msp "running"
  threadDelay $ 5 * 1000000
  killThread threadId
