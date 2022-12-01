module UI.PlayerBoard (
    drawPlayerBoard,
    handleKeyPlayer,
    sendMess
) where

import Game
import Codenames
import UI.Styles

import Control.Concurrent       
import Control.Monad.IO.Class

import Brick
import Network.Socket
import qualified Brick.Main as M
import Network.Socket.ByteString (recv, sendAll)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BW
import qualified Data.ByteString.Char8 as CH

-- | Get the background style of a card based on its color
getColorBgStyle :: CardColor -> AttrName
getColorBgStyle Red = styleRedCell
getColorBgStyle Blue = styleBlueCell
getColorBgStyle Black = styleBlackCell
getColorBgStyle Yellow = styleYellowCell

-- | Render a clicked card on which player cursor is present currently
getClickedCursorStyle :: String -> CardColor -> Widget Name
getClickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

-- | Render an unclicked card on which player cursor is present currently
getUnclickedCursorStyle :: String -> CardColor -> Widget Name
getUnclickedCursorStyle word _ = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

-- | Render a clicked card on which player cursor is not present
getClickedNormalStyle :: String -> CardColor -> Widget Name
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

-- | Render an unclicked card on which player cursor is not present
getUnclickedNormalStyle ::  String -> CardColor -> Widget Name
getUnclickedNormalStyle word _ = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

-- | Render a single player card
drawPlayerCard :: PlayerCell -> Coord -> Widget Name
drawPlayerCard (PCell (Loc cardx cardy) word True color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle word color
                                                                                    else getClickedNormalStyle word color
                                                                                        
drawPlayerCard (PCell (Loc cardx cardy) word False color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getUnclickedCursorStyle word color
                                                                                    else getUnclickedNormalStyle word color
-- | Render the player side game board
drawGrid :: PlayerGameState -> Widget Name
drawGrid pb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Player View")
    $ vBox rows
    where
        currCursor = pPlayerCursor pb
        rows = [hBox $ (cardsInRow r) | r <- (playerGrid pb)]
        cardsInRow row = [vLimit 30 $ hLimit 25 $ (drawPlayerCard pcard currCursor) | pcard <- row]

-- | Render hint given by the spymaster
renderHint :: String -> Int -> Widget Name
renderHint hintW hintNumW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ (str (hintW ++ "," ++ (show hintNumW)))

-- | Render red team's score
getRedTeamScoreBoard :: Int -> Widget Name
getRedTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleRedCell $ str ("Team Red score :" ++ (show score))

-- | Render blue team's score
getBlueTeamScoreBoard :: Int -> Widget Name
getBlueTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleBlueCell $ str ("Team Blue score :" ++ (show score))

-- | Render which team's turn it is currently
renderPlayerTurn :: CardColor -> Widget Name
renderPlayerTurn playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Team's Turn")

-- | Render the player side stats for each team
drawPlayerStats :: PlayerGameState -> Widget Name
drawPlayerStats pb = ((getBlueTeamScoreBoard (pBlueTeamScore pb)) <=> (getRedTeamScoreBoard (pRedTeamScore pb))) <+> ((padLeft Max (renderHint hintWord hintNum)) <=> (padLeft Max (renderPlayerTurn (pTeamTurn pb))))
    where SHint hintWord hintNum = pSpyHint pb

-- | Render the player side view
drawPlayerBoard :: PlayerGameState -> [Widget Name]
drawPlayerBoard pb = [(drawGrid pb) <=> (drawPlayerStats pb)]


-- send and recieve message APIs
sendMess :: Socket -> String -> IO ()
sendMess sock s = do
--   sock <- openConnection
  sendAll sock $ CH.pack s


-- | Handle key events on the player view
handleKeyPlayer :: Codenames -> BrickEvent Name ConnectionTick -> EventM Name (Next Codenames)
handleKeyPlayer (PlayerView playerGameState) (VtyEvent (V.EvKey key [])) =
  case key of
    V.KUp    -> M.continue $ (PlayerView  $ (moveCursor UpD playerGameState))
    V.KDown  -> M.continue $ (PlayerView  $ (moveCursor DownD playerGameState))
    V.KLeft  -> M.continue $ (PlayerView  $ (moveCursor LeftD playerGameState))
    V.KRight -> M.continue $ (PlayerView  $ (moveCursor RightD playerGameState))
    V.KEnter -> do
                    liftIO $ (sendMess (pSock playerGameState) (show (pPlayerCursor playerGameState)))
                    M.continue $ (PlayerView  $ (updateGame playerGameState))
    V.KEsc   -> halt (PlayerView playerGameState)
    _        -> M.continue $ (PlayerView playerGameState)
-- handleKeyPlayer playerGameState _ = M.continue $ (PlayerView playerGameState)


handleKeyPlayer (PlayerView playerGameState) (AppEvent (ConnectionTick csReceived)) = do
                                case csReceived of
                                    S_Str message ->  continue $ (PlayerView (updateHintFromSpy message playerGameState))



handleKeyPlayer (PlayerView playerGameState) _ = M.continue (PlayerView playerGameState)

handleKeyPlayer _ _ = undefined