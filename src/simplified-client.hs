module Main where


import Control.Concurrent       
import Control.Monad  
import qualified Brick.Main as M
import Brick.BChan
import Network.Socket 
import Data.List.Split
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)
import Graphics.Vty
import qualified Data.ByteString.Char8 as C

import qualified Data.Text as T
import System.Random

import qualified Data.Text.IO as Text


import Codenames
import Game
import UI.PlayerBoard
import UI.Styles
import UI.GameUI

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

getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)

main :: IO ()
main = do
  print "Inside client:"
  sock <- openConnection
  let buildVty = do
                      v <- mkVty =<< standardIOConfig
                      return v
  initialVty <- buildVty
  recvFileNum <- recvMess sock
  eventChan <- Brick.BChan.newBChan 10
  reader <- forkIO $ fix $ \loop -> do
              message <- recvMess sock
              writeBChan eventChan $ ConnectionTick (S_Str message)
              loop
  wordLis <- getWords ("resources/words_files/" ++ recvFileNum  ++".txt")
  colorLis <- getWords ("resources/colors_files/" ++ recvFileNum  ++".txt")
  endState <- M.customMain initialVty buildVty (Just eventChan) playerApp (PlayerView (createPlayerState wordLis colorLis sock))
  return ()

-- | Brick app for handling a codenames game
playerApp :: M.App Codenames ConnectionTick Hint
playerApp =
  M.App
    { M.appDraw = drawGame,
      M.appChooseCursor = const . const Nothing,
      M.appHandleEvent = handleKeyPlayer,
      M.appStartEvent = return,
      M.appAttrMap = const attributes
    }

--   return ()
