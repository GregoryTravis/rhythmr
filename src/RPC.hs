{-# LANGUAGE BlockArguments #-}

-- Code lifted from https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

module RPC (rpc) where

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
--import Network.Socket hiding (recv)
--import Network.Socket (withFdSocket)
import Network.Socket.ByteString (recv, sendAll)

import Util

-- todo
-- move this to TCP.hs
-- withServer wrapper so we can clean it up
-- Use a chan
-- withRemote wrapper which provides the client chan
-- Open a gfx window in the server
-- only draw the circle when the command comes

rpc = do
  forkIO $ do
    threadDelay $ 1 * 1000000
    rpcc
  threadId <- forkIO $ runServer
  msp "running"
  threadDelay $ 5 * 1000000
  killThread threadId

runServer = do
  runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024000000
        msp $ C.unpack msg
        unless (S.null msg) $ do
          sendAll s msg
          talk s

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)-- import Network.Socket

rpcc :: IO ()
rpcc = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s $ C.pack "Hello, world!"
    msg <- recv s 1024000000
    putStr "Received: "
    C.putStrLn msg

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

-- import Util

-- rpc = do
--     sock <- socket AF_INET Stream 0    -- create socket
--     setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
--     bind sock (SockAddrInet 4242 iNADDR_ANY)   -- listen on TCP port 4242.
--     listen sock 2                              -- set a max of 2 queued connections
--     mainLoop sock                              -- unimplementedrpc = do
--     msp "rpc hi"

-- mainLoop :: Socket -> IO ()
-- mainLoop sock = do
--     conn <- accept sock     -- accept a connection and handle it
--     runConn conn            -- run our server's logic
--     mainLoop sock           -- repeat

-- runConn :: (Socket, SockAddr) -> IO ()
-- runConn (sock, _) = do
--     send sock "Hello!\n"
--     close sock
