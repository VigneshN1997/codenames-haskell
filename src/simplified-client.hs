module Main where

import qualified Data.ByteString.Char8 as C
import Data.List.Split
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent (threadDelay)

-- send and recieve message APIs
sendMess :: Socket -> String -> IO ()
sendMess sock s = do
--   sock <- openConnection
  sendAll sock $ C.pack s

recvMess :: Socket -> IO [Char]
recvMess sock = do
--   sock <- openConnection
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
  case inp of
    "S" -> do
      print ("Recieved input: " ++ inp)
      sendMess sock "Enter2,2"
      sendMess sock "Next Msg from client"
      loopTry sock
    "R" -> do
      print ("Recieved input: " ++ inp)
      message <- recvMess sock
      let splitString = splitOn "," message
      print ("Client:loopTry recieved  Clue: " ++ splitString !! 0 ++ "  Number : " ++ splitString !! 1)
      loopTry sock
    _ -> do
      print ("Recieved input: " ++ inp)
      return ()

recurConv :: Socket -> IO()
recurConv sock = do
                sendMess sock "Initial Msg from client"
                message <- recvMess sock
                print ("Client: " ++ message)
                threadDelay 10000
                recurConv sock

main :: IO ()
main = do
  print "Inside client:"
  sock <- openConnection
  recurConv sock
  --     loopTry sock
  return ()

--   main

--   return ()
