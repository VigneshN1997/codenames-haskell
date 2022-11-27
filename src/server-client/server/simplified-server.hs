module Main where

import Control.Concurrent       
import Control.Monad             
-- import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C

import Network.Socket 
import Data.List.Split
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)

-- helper function to obtain a socket connection
openConnection :: IO Socket
openConnection = do
                  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
                  let serveraddr = head addrinfos
                  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                  connect sock (addrAddress serveraddr)
                  return sock

-- TODO: There is redundancy between main and openConnection. Need to Refactor

setupServer :: IO()
setupServer = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5
  runTCPEchoServerForever sock 0

sendMess :: Socket -> String -> IO ()
sendMess sock s = do
                -- sock <- openConnection
                sendAll sock $ C.pack s


recvMess :: Socket -> IO [Char]
recvMess sock = do
    -- sock <- openConnection
    x <- recv sock 1024
    return (C.unpack x)

main :: IO ()
main = do
        setupServer




runTCPEchoServerForever :: (Eq a, Num a) => Socket -> a -> IO b
runTCPEchoServerForever sock msgNum = do 
  (conn, _)     <- accept sock
  _ <- forkIO (rrLoop conn msgNum) -- conn and sock are same
  runTCPEchoServerForever sock $! msgNum + 1

rrLoop :: Eq a => Socket -> a -> IO ()
rrLoop sock msgNum = do
  msg <- recv sock 1024
  let s = C.unpack msg
  print s
  sendAll sock $ C.pack "dwdwdw"
  -- sendMess sock "Loc 0,0"
  print "sup?"
  rrLoop sock 3



  -- reader <- forkIO $ fix $ \loop -> do
  --       -- s <- getLine
  --       -- sendAll sock (C.pack "Temperature,2")
  --       sendAll sock (C.pack s)
  --       -- sendAll sock (C.pack "Some message from server!")
  --       -- threadDelay 5000000
  --       -- loop


  -- writer sock
  -- killThread reader

  -- helper function for listening to the client
-- writer :: Socket -> IO ()
-- writer sock = do
--                 -- sock <- getSocket
--                 msg <- recv sock 1024
--                 let s = C.unpack msg
--                 case s of
--                   "quit" -> do
--                               print "TCP client closing"
--                               threadDelay 100000
--                               -- close sock
--                   ""     -> return ()
--                   _     -> do
--                               let splitString = splitOn "Enter" s                                
--                               print ("TCP server received: " ++ splitString!!1)
--                               threadDelay 100000
                              -- writer



-- To split string on delimiter : let splitString = splitOn "," message

