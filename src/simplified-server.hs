module Main where

import Control.Concurrent       
import Control.Monad             
-- import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C

import qualified Brick.Main as M
import Brick.BChan
import Network.Socket 
import Data.List.Split
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)
import Graphics.Vty

import Codenames
import Game
import UI.SpyBoard
import UI.Styles
import UI.GameUI
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import System.Random

import qualified Data.Text.IO as Text
import qualified Data.ByteString.Char8 as CH

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
  return conn

-- Loads words from a text file into a list.
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)

sendMess :: Socket -> String -> IO ()
sendMess sock s = do
  sendAll sock $ CH.pack s

main :: IO ()
main = do
        comm_sock <- setupServer
        let buildVty = do
                      v <- mkVty =<< standardIOConfig
                      return v
        initialVty <- buildVty
        filenum <- randomRIO (1, 16) :: IO Int
        _ <- sendMess comm_sock (show filenum)
        eventChan <- Brick.BChan.newBChan 10
        reader <- forkIO $ fix $ \loop -> do
                message <- recvMess comm_sock
                writeBChan eventChan $ ConnectionTick (S_Str message)
                loop
        
        wordLis <- getWords ("resources/words_files/" ++ (show filenum)  ++".txt")
        colorLis <- getWords ("resources/colors_files/" ++ (show filenum)  ++".txt")
        endState <- M.customMain initialVty buildVty (Just eventChan) spyApp (SpyView (SpyStateAndForm {_wordCount = E.editor WordCountField (Just 1) (T.pack ""), _spyState = (createSpyState wordLis colorLis comm_sock)})) 
        return ()


-- | Brick app for handling a codenames game
spyApp :: M.App Codenames ConnectionTick Hint
spyApp =
  M.App
    { M.appDraw = drawGame,
      M.appChooseCursor = const . const Nothing,
      M.appHandleEvent = handleSEvent,
      M.appStartEvent = return,
      M.appAttrMap = const attributes
    }


recvMess :: Socket -> IO [Char]
recvMess sock = do
    -- sock <- openConnection
    x <- recv sock 1024
    return (C.unpack x)