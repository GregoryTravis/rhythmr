-- Code lifted from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

module RPC (rpc) where

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)

import ClientServer
import Util

handler s = return $ s ++ ":" ++ s

rpc = do
  forkIO $ do
    threadDelay $ 1 * 1000000
    withRPCClient "127.0.0.1" 3000 $ \c -> do
      resp <- rpcSend c "hey now"
      msp $ "Got " ++ resp
      threadDelay $ 1 * 1000000
      resp' <- rpcSend c "hey now2"
      msp $ "Got " ++ resp'
  threadId <- forkIO $ rpcServer 3000 handler
  msp "running"
  threadDelay $ 5 * 1000000
  killThread threadId

data RPCClient = RPCClient Client

rpcSend :: (Show a, Read b) => RPCClient -> a -> IO b
rpcSend (RPCClient client) request = do
  s <- socketSend client (show request)
  return (read s)

withRPCClient :: String -> Int -> (RPCClient -> IO a) -> IO a
withRPCClient host port handler = withSocketClient host port $ \s -> handler (RPCClient s)

rpcServer :: (Read a, Show b) => Int -> (a -> IO b) -> IO ()
rpcServer port handler = socketServer port rpcHandler
  where rpcHandler x = fmap show $ handler (read x)
  --where rpcHandler x = return $ show $ handler (read x)
