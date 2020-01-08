-- Code lifted from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

module RPC (rpc) where

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)

import ClientServer
import Util

handler s = return $ s ++ ":" ++ s

rpc = do
  forkIO $ do
    threadDelay $ 1 * 1000000
    withClient "127.0.0.1" 3000 $ \c -> do
      resp <- send c "hey now"
      msp $ "Got " ++ resp
  threadId <- forkIO $ server 3000 handler
  msp "running"
  threadDelay $ 5 * 1000000
  killThread threadId
