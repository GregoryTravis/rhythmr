-- Code lifted from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

module RPC
( RPCClient
, rpc 
, rpcSend
, withRPCClient
, rpcServer ) where

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)

import ClientServer
import Util

handler s = return $ s ++ ":" ++ s

-- clientHandler :: Client -> IO ()
-- clientHandler c = do
--   Bar y = rpcSend c (Foo 12)
--   msp y

serverHandler (Foo x) = Bar (x*2)

rpc = do
  forkIO $ do
    threadDelay $ 1 * 1000000
    withRPCClient "127.0.0.1" 3000 $ \c -> do
      resp <- rpcSend c (Foo 12)
      msp $ "Got " ++ resp
  threadId <- forkIO $ rpcServer 3000 (return . serverHandler)
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
