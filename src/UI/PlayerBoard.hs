module UI.PlayerBoard (
    drawPlayerBoard,
    handleKeyPlayer,
    sendMess,
    drawKeyInstructions
) where


import Brick
import Control.Monad.IO.Class
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BW

import Game
import Codenames
import UI.Styles
import Common

playerInst :: [[Char]]
playerInst = [ "move:    ←↓↑→ "
              , "select word:  enter"
              , "end turn: e"
              , "quit:    esc"
              ]

-- | Get the background style of a card based on its color
getColorBgStyle :: CardColor -> AttrName
getColorBgStyle Red = styleRedCell
getColorBgStyle Blue = styleBlueCell
getColorBgStyle Black = styleBlackCell
getColorBgStyle Yellow = styleYellowCell

-- | Render a clicked card on which player cursor is present currently
getClickedCursorStyle :: String -> CardColor -> Widget Hint
getClickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

-- | Render an unclicked card on which player cursor is present currently
getUnclickedCursorStyle :: String -> CardColor -> Widget Hint
getUnclickedCursorStyle word _ = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

-- | Render a clicked card on which player cursor is not present
getClickedNormalStyle :: String -> CardColor -> Widget Hint
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

-- | Render an unclicked card on which player cursor is not present
getUnclickedNormalStyle ::  String -> CardColor -> Widget Hint
getUnclickedNormalStyle word _ = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

-- | Render a single player card
drawPlayerCard :: PlayerCell -> Coord -> Widget Hint
drawPlayerCard (PCell (Loc cardx cardy) word True color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle word color
                                                                                    else getClickedNormalStyle word color

drawPlayerCard (PCell (Loc cardx cardy) word False color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getUnclickedCursorStyle word color
                                                                                    else getUnclickedNormalStyle word color
-- | Render the player side game board
drawGrid :: PlayerGameState -> Widget Hint
drawGrid pb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Player View")
    $ vBox rows
    where
        currCursor = pPlayerCursor pb
        rows = [hBox $ (cardsInRow r) | r <- (playerGrid pb)]
        cardsInRow row = [vLimit 30 $ hLimit 25 $ (drawPlayerCard pcard currCursor) | pcard <- row]

-- | Render hint given by the spymaster
renderHint :: CardColor -> String -> Widget Hint
renderHint Red _ = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr (getColorBgStyle Red) $ str redWonStr
renderHint Blue _ = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr (getColorBgStyle Blue) $ str blueWonStr
renderHint _ hintW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ (str hintW)

-- | Render red team's score
getRedTeamScoreBoard :: Int -> Widget Hint
getRedTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleRedCell $ str ("Team Red Cards left :" ++ (show score))

-- | Render blue team's score
getBlueTeamScoreBoard :: Int -> Widget Hint
getBlueTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleBlueCell $ str ("Team Blue Cards left :" ++ (show score))


-- | render which team's turn is it currently
renderTurn :: Bool -> CardColor -> Widget Hint
renderTurn False playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Teams's Turn")
renderTurn True playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Spy's Turn")


-- | Render the player side stats for each team
drawPlayerStats :: PlayerGameState -> Widget Hint
drawPlayerStats pb = (getBlueTeamScoreBoard (pBlueTeamScore pb) <=> getRedTeamScoreBoard (pRedTeamScore pb)) <+> (padLeft Max (renderHint (pWinner pb) hintWordCount) <=> padLeft Max (renderTurn (pSpyMastersTurn pb) (pTeamTurn pb)))
    where hintWordCount = pSpyHint pb

-- | Draw the logs of player selected cards in order
drawLogs :: PlayerGameState -> Widget Hint
drawLogs pb = (setAvailableSize (50, 20)) $ (withBorderStyle BS.unicodeBold) $ (B.borderWithLabel (str " Logs ")) $  (padLeftRight 1) $ str $  unlines $ (pLogs pb)

-- | Render the player side view
drawPlayerBoard :: PlayerGameState -> [Widget Hint]
drawPlayerBoard pb = [(drawGrid pb) <=> (drawPlayerStats pb) <=> (drawKeyInstructions <+> (drawLogs pb))]

-- | Render the player instructions to use keys to play game
drawKeyInstructions :: Widget Hint
drawKeyInstructions = (setAvailableSize (31, 12)) $ (withBorderStyle BS.unicodeBold) $ (B.borderWithLabel (str " Help ")) $  (padLeftRight 1) $ str $  unlines $ playerInst  



-- | Handle key events on the player view
handleKeyPlayer :: Codenames -> BrickEvent Hint ConnectionTick -> EventM Hint (Next Codenames)
handleKeyPlayer (PlayerView playerGameState) (VtyEvent (V.EvKey key [])) =
  case key of
    V.KUp    -> M.continue $ (PlayerView  $ (moveCursor UpD playerGameState))
    V.KDown  -> M.continue $ (PlayerView  $ (moveCursor DownD playerGameState))
    V.KLeft  -> M.continue $ (PlayerView  $ (moveCursor LeftD playerGameState))
    V.KRight -> M.continue $ (PlayerView  $ (moveCursor RightD playerGameState))
    V.KEnter -> if (pWait playerGameState)
                    then M.continue $ (PlayerView  playerGameState)
                    else
                      do
                          liftIO $ (sendMess (pSock playerGameState) (show (pPlayerCursor playerGameState)))
                          M.continue $ PlayerView  $ updateGame playerGameState
    V.KEsc   -> do
                  liftIO $ sendMess (pSock playerGameState) "quit" 
                  halt (PlayerView playerGameState)
    V.KChar 'e' -> do
                    liftIO $ sendMess (pSock playerGameState) "end"
                    M.continue $ PlayerView  $ endTurn playerGameState

    _        -> M.continue $ PlayerView playerGameState

-- | Handle brick app events (from brick channel)
handleKeyPlayer (PlayerView playerGameState) (AppEvent (ConnectionTick csReceived)) = do
                                case csReceived of
                                    S_Str message ->  continue $ (PlayerView (updateHintFromSpy message playerGameState))



handleKeyPlayer (PlayerView playerGameState) _ = M.continue (PlayerView playerGameState)

handleKeyPlayer _ _ = undefined


