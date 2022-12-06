module Common (
    getWords
    , sendMess
    , recvMess
) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as CH


-- Loads words from a text file into a list.
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)

sendMess :: Socket -> String -> IO ()
sendMess sock s = do
  sendAll sock $ CH.pack s

recvMess :: Socket -> IO [Char]
recvMess sock = do
    -- sock <- openConnection
    x <- recv sock 1024
    return (C.unpack x)