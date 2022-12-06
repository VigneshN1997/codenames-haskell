module Main where

import Control.Concurrent 
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad.Fix (fix)


sendMess :: Socket -> String -> IO (Maybe String)
sendMess sock s = do
              sendAll sock $ C.pack s
              return Nothing


recvMess :: Socket -> IO [Char]
recvMess sock = do
    x <- recv sock 1024
    return (C.unpack x)


openConnection :: IO Socket
openConnection = do
                  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
                  let serveraddr = head addrinfos
                  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                  connect sock (addrAddress serveraddr)
                  return sock

loopTry :: Socket -> IO ()
loopTry sock = do 
            inp <- getLine
            print "BBBB"
            print inp
            case inp of
                "N" -> return ()
                "Y"  -> do
                        message <- recvMess sock
                        print message
                        loopTry sock


main :: IO ()
main = do
            print "HELLO"
            putStrLn "HELLO"
            sock <- openConnection
            -- eventChan <- newChan
            message <- recvMess sock
            print message
            loopTry sock
                        
                
                
            -- reader <- forkIO $ fix $ \loop -> do
            --   message <- recvMess sock
            --   print "HELLO"
            --   putStrLn "HELLO"
            --   putStrLn message
            --   print message
            --   loop
            -- _ <- getLine
            return ()


-- module Main where

-- import Network.Socket
-- import System.IO
-- import Control.Exception
-- import Control.Concurrent
-- import Control.Monad (when)
-- import Control.Monad.Fix (fix)

-- main :: IO ()
-- main = do
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   bind sock (SockAddrInet 4242 0x0100007f)
--   listen sock 2
--   chan <- newChan
--   _ <- forkIO $ fix $ \loop -> do
--     (_, _) <- readChan chan
--     loop
--   mainLoop sock chan 0

-- type Msg = (Int, String)

-- mainLoop :: Socket -> Chan Msg -> Int -> IO ()
-- mainLoop sock chan msgNum = do
--   conn <- accept sock
--   forkIO (runConn conn chan msgNum)
--   mainLoop sock chan $! msgNum + 1


-- convertStringToInt :: String -> Int
-- convertStringToInt s  = read s

-- -- convertIntToString :: Int -> String
-- -- convertIntToString x = read x

-- sendClue :: String -> IO ()
-- sendClue = undefined

-- sendLoc :: String -> IO ()
-- sendLoc = undefined

-- runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
-- runConn (sock, _) chan msgNum = do
--     let broadcast msg = writeChan chan (msgNum, msg)
--     hdl <- socketToHandle sock ReadWriteMode
--     hSetBuffering hdl NoBuffering

--     hPutStrLn hdl "Blue team, clue: Apple 3"
--     name <- fmap init (hGetLine hdl)
--     -- broadcast ("--> " ++ name ++ " entered chat.")

--     hPutStrLn hdl ("Welcome, " ++ name  ++ "!")

--     -- commLine <- dupChan chan

--     -- fork off a thread for reading from the duplicated channel
--     -- reader <- forkIO $ fix $ \loop -> do
--     --     (nextNum, line) <- readChan commLine
--     --     when (msgNum /= nextNum) $ hPutStrLn hdl line
--     --     loop

--     handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
--         line <- fmap init (hGetLine hdl)
--         case line of
--              -- If an exception is caught, send a message and break the loop
--              "quit" -> hPutStrLn hdl "Bye!"
--              -- else, continue looping.
--              _      -> broadcast (name ++ ": " ++ line) >> loop

--     -- killThread reader                      -- kill after the loop ends
--     broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
--     hClose hdl                             -- close the handle


