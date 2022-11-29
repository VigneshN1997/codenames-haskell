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


import Codenames
import Game
import UI.PlayerBoard
import UI.Styles
import UI.GameUI

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

-- loopTry :: Socket -> IO ()
-- loopTry sock = do
--   inp <- getLine
--   case inp of
--     "S" -> do
--       print ("Recieved input: " ++ inp)
--       sendMess sock "Enter2,2"
--       sendMess sock "Next Msg from client"
--       loopTry sock
--     "R" -> do
--       print ("Recieved input: " ++ inp)
--       message <- recvMess sock
--       let splitString = splitOn "," message
--       print ("Client:loopTry recieved  Clue: " ++ splitString !! 0 ++ "  Number : " ++ splitString !! 1)
--       loopTry sock
--     _ -> do
--       print ("Recieved input: " ++ inp)
--       return ()

-- recurConv :: Socket -> IO()
-- recurConv sock = do
--                 sendMess sock "Initial Msg from client"
--                 message <- recvMess sock
--                 print ("Client: " ++ message)
--                 threadDelay 10000
--                 recurConv sock

main :: IO ()
main = do
  print "Inside client:"
  sock <- openConnection
  let buildVty = do
                      v <- mkVty =<< standardIOConfig
                      return v
  initialVty <- buildVty
  eventChan <- Brick.BChan.newBChan 10
  reader <- forkIO $ fix $ \loop -> do
              message <- recvMess sock
              writeBChan eventChan $ ConnectionTick (S_Str message)
              loop
  endState <- M.customMain initialVty buildVty (Just eventChan) playerApp (PlayerView (createPlayerState egwordList downloadedColorList sock))
  return ()

-- | Brick app for handling a codenames game
playerApp :: M.App Codenames ConnectionTick Name
playerApp =
  M.App
    { M.appDraw = drawGame,
      M.appChooseCursor = const . const Nothing,
      M.appHandleEvent = handleKeyPlayer,
      M.appStartEvent = return,
      M.appAttrMap = const attributes
    }

--   return ()
