module Main where

import qualified Data.ByteString.Char8 as C
import Data.List.Split
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

-- send and recieve message APIs
sendMess :: String -> IO ()
sendMess s = do
                sock <- openConnection
                sendAll sock $ C.pack s


recvMess :: IO [Char]
recvMess = do
    sock <- openConnection
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
                        sendMess "Enter2,2"
                        -- sendMess "Next Msg from client"
                        loopTry sock
                "R"  -> do
                        print ("Recieved input: " ++ inp)
                        message <- recvMess
                        let splitString = splitOn "," message
                        print ("Client:loopTry recieved  Clue: " ++ splitString!!0 ++ "Number : "++ splitString!!1)
                        loopTry sock
                _ -> do
                        print ("Recieved input: " ++ inp)
                        return ()


main :: IO ()
main = do
            print "Inside client:"
            sock <- openConnection
            message <- recvMess
            print ("Client: " ++ message)
            sendMess "Initial Msg from client"
            loopTry sock
                        
            return ()

