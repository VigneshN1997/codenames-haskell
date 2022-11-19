module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0x0100007f)
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle






-- Echo server program
-- module Main (main) where

-- import Control.Concurrent (forkFinally)
-- import qualified Control.Exception as E
-- import Control.Monad (unless, forever, void)
-- import qualified Data.ByteString as S
-- import Network.Socket
-- import Network.Socket.ByteString (recv, sendAll)

-- main :: IO ()
-- main = runTCPServer Nothing "3000" talk
--   where
--     talk s = do
--         msg <- recv s 1024
--         unless (S.null msg) $ do
--           sendAll s msg
--           talk s

-- -- from the "network-run" package.
-- runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
-- runTCPServer mhost port server = withSocketsDo $ do
--     addr <- resolve
--     E.bracket (open addr) close loop
--   where
--     resolve = do
--         let hints = defaultHints {
--                 addrFlags = [AI_PASSIVE]
--               , addrSocketType = Stream
--               }
--         head <$> getAddrInfo (Just hints) mhost (Just port)
--     open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
--         setSocketOption sock ReuseAddr 1
--         withFdSocket sock setCloseOnExecIfNeeded
--         bind sock $ addrAddress addr
--         listen sock 1024
--         return sock
--     loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
--         $ \(conn, _peer) -> void $
--             -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
--             -- but 'E.bracketOnError' above will be necessary if some
--             -- non-atomic setups (e.g. spawning a subprocess to handle
--             -- @conn@) before proper cleanup of @conn@ is your case
--             forkFinally (server conn) (const $ gracefulClose conn 5000)
