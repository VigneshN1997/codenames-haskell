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

setupServer :: IO Socket
setupServer = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5
  (conn, _)     <- accept sock
  -- runTCPEchoServerForever sock 0
  print conn
  return conn


sendMess :: Socket -> String -> IO ()
sendMess sock s = do
                -- sock <- openConnection
                sendAll sock $ C.pack s


recvMess :: Socket -> IO [Char]
recvMess sock = do
    -- sock <- openConnection
    x <- recv sock 1024
    return (C.unpack x)

talkToClient :: Socket -> IO()
talkToClient sock = do 
                      print sock
                      mes <- recvMess sock
                      print mes
                      sendMess sock "HELLOOOOO"
                      -- _ <- getLine
                      -- threadDelay 2000000
                      talkToClient sock

main :: IO ()
main = do
        sock <- setupServer
        talkToClient sock





runTCPEchoServerForever :: (Eq a, Num a) => Socket -> a -> IO b
runTCPEchoServerForever sock msgNum = do 
  (conn, _)     <- accept sock
  _ <- forkIO (rrLoop conn) -- conn and sock are same
  _ <- forkIO (wLoop conn)
  runTCPEchoServerForever sock $! msgNum + 1

wLoop :: Socket -> IO ()
wLoop sock = do
    sendAll sock $ C.pack "dwdwdw"
    print "wLoop: completed writing"
    threadDelay 5000000
    wLoop sock

rrLoop ::Socket -> IO ()
rrLoop sock = do
  msg <- recv sock 1024
  let s = C.unpack msg
  print s

  -- TODO: write into the brick channel here

  
  print "rrLoop: completed listening"
  rrLoop sock

