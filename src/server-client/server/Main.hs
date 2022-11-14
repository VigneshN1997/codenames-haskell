-- in Main.hs
-- module Main where

-- import qualified Data.ByteString.Char8 as C
-- import Network.Socket
-- import Network.Socket.ByteString (recv, sendAll)

-- main :: IO ()
-- main = do
--     addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4545")
--     let serveraddr = head addrinfos
--     sock <- socket AF_INET Stream 0    -- create socket
--     setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
--     bind sock (SockAddrInet 4242 0x0100007f)   -- listen on TCP port 4242.
--     listen sock 2                              -- set a max of 2 queued connections
--     mainLoop sock                              -- unimplemented

-- mainLoop :: Socket -> IO ()
-- mainLoop sock = do
--     conn <- accept sock     -- accept a connection and handle it
--     runConn conn            -- run our server's logic
--     mainLoop sock           -- repeat

-- runConn :: (Socket, SockAddr) -> IO ()
-- runConn (sock, _) = do
--     sendAll sock (C.pack "Hello!\n")
--     -- close sock



-- Echo server program
module Main (main) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
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
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)
