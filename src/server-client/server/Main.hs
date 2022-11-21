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





module Main where

import Control.Concurrent       
import Control.Monad             
-- import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)

-- import System.Directory

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5
  chan <- newChan
  runTCPEchoServerForever sock chan 0

runTCPEchoServerForever :: (Eq a, Num a) => Socket -> Chan (a, String) -> a -> IO b
runTCPEchoServerForever sock chan msgNum = do 
  (conn, _)     <- accept sock
  -- files_list    <- getFileList
  -- sendAll conn (C.pack files_list)
  -- threadDelay 100000
  _ <- forkIO (rrLoop conn chan msgNum) -- conn and sock are same
  runTCPEchoServerForever sock chan $! msgNum + 1

-- getFileList :: IO String
-- getFileList = do
--   setCurrentDirectory "shared_files"
--   _cd <- getCurrentDirectory
--   _file <- getDirectoryContents _cd
--   onlyFiles <- filterM doesFileExist _file
--   let mes = convertListToString onlyFiles ""
--   setCurrentDirectory ".."
--   return mes


convertListToString :: [FilePath] -> String -> String
convertListToString [] x = init x
convertListToString (n:ns) x = convertListToString ns (x ++ n ++ ",")

rrLoop :: Eq a => Socket -> Chan (a, String) -> a -> IO ()
rrLoop sock chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)
  
  broadcast "--> AAAAA new person entered chat"
  print "new person entered chat"

  -- commLine <- dupChan chan

  reader <- forkIO $ fix $ \loop -> do
        print "AAAAAA"
        sendAll sock (C.pack "CHENNNAAAAAAAAAAA!")
        -- (nextNum, mes) <- readChan commLine
        -- print "BBBBBBB"
        -- when (msgNum /= nextNum) $ sendAll sock (C.pack mes)
        -- print "CCCCCC"
        threadDelay 5000000
        loop

  writer sock broadcast
  killThread reader

writer :: Socket -> ([Char] -> IO a) -> IO ()
writer sock broadcast = do
                        msg <- recv sock 1024
                        let s = C.unpack msg
                        case s of
                          "quit" -> do
                                      print "TCP client closing"
                                      threadDelay 100000
                                      close sock
                          ""     -> return ()
                          _      -> do
                                      broadcast s
                                      print ("TCP server received: " ++ s)
                                      threadDelay 100000
                                      writer sock broadcast