module Main where

import Control.Concurrent

import Brick.BChan
import Network.Socket
import System.IO()
import Control.Monad.Fix (fix)
import Graphics.Vty
import System.Random
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T


import Codenames
import Game
import UI.SpyBoard
import UI.Styles
import UI.GameUI
import Common


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
        _ <- forkIO $ fix $ \loop -> do
                message <- recvMess comm_sock
                writeBChan eventChan $ ConnectionTick (S_Str message)
                loop
        
        wordLis <- getWords ("resources/words_files/" ++ (show filenum)  ++".txt")
        colorLis <- getWords ("resources/colors_files/" ++ (show filenum)  ++".txt")
        _ <- M.customMain initialVty buildVty (Just eventChan) spyApp (SpyView (SpyStateAndForm {_wordCount = E.editor WordCountField (Just 1) (T.pack ""), _spyState = (createSpyState wordLis colorLis comm_sock)})) 
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

