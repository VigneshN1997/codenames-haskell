module Main where

import Control.Concurrent       
import Control.Monad             
-- import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 5
  runTCPEchoServerForever sock 0

runTCPEchoServerForever :: (Eq a, Num a) => Socket -> a -> IO b
runTCPEchoServerForever sock msgNum = do 
  (conn, _)     <- accept sock
  _ <- forkIO (rrLoop conn msgNum) -- conn and sock are same
  runTCPEchoServerForever sock $! msgNum + 1

rrLoop :: Eq a => Socket -> a -> IO ()
rrLoop sock msgNum = do  
  print "new person entered chat"

  reader <- forkIO $ fix $ \loop -> do
        sendAll sock (C.pack "Some message!")
        threadDelay 5000000
        loop

  writer sock
  killThread reader

writer :: Socket -> IO ()
writer sock = do
                        msg <- recv sock 1024
                        let s = C.unpack msg
                        case s of
                          "quit" -> do
                                      print "TCP client closing"
                                      threadDelay 100000
                                      close sock
                          ""     -> return ()
                          _      -> do
                                      print ("TCP server received: " ++ s)
                                      threadDelay 100000
                                      writer sock