module Main where


import Control.Concurrent
import Brick.BChan
import Network.Socket
import System.IO()
import Control.Monad.Fix (fix)
import Graphics.Vty
import qualified Brick.Main as M


import Codenames
import Game
import UI.PlayerBoard
import UI.Styles
import UI.GameUI
import Common


openConnection :: IO Socket
openConnection = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock

main :: IO ()
main = do
  sock <- openConnection
  let buildVty = do
                      v <- mkVty =<< standardIOConfig
                      return v
  initialVty <- buildVty
  recvFileNum <- recvMess sock
  eventChan <- Brick.BChan.newBChan 10
  _ <- forkIO $ fix $ \loop -> do
              message <- recvMess sock
              writeBChan eventChan $ ConnectionTick (S_Str message)
              loop
  wordLis <- getWords ("resources/words_files/" ++ recvFileNum  ++".txt")
  colorLis <- getWords ("resources/colors_files/" ++ recvFileNum  ++".txt")
  _ <- M.customMain initialVty buildVty (Just eventChan) playerApp (PlayerView (createPlayerState wordLis colorLis sock))
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
