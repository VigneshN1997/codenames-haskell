module UI.SpyBoard (
    drawSpyBoard,
    handleSEvent
) where

import Game
import Codenames
import UI.Styles

import Brick
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BW


getColorBgStyle :: CardColor -> AttrName
getColorBgStyle Red = styleRedCell
getColorBgStyle Blue = styleBlueCell
getColorBgStyle Black = styleBlackCell
getColorBgStyle Yellow = styleYellowCell

getClickedCursorStyle :: String -> CardColor -> Widget Name
getClickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

getUnclickedCursorStyle :: String -> CardColor -> Widget Name
getUnclickedCursorStyle word _ = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

getClickedNormalStyle :: String -> CardColor -> Widget Name
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word


getUnclickedNormalStyle ::  String -> CardColor -> Widget Name
getUnclickedNormalStyle word _ = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

drawSpyCard :: SpyCell -> Coord -> Widget Name
drawSpyCard (SCell (Loc cardx cardy) word _ color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle word color
                                                                                    else getClickedNormalStyle word color
                                                                                        

drawGrid :: SpyGameState -> Widget Name
drawGrid sb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Spy View")
    $ vBox rows
    where
        currCursor = sPlayerCursor sb
        rows = [hBox $ (cardsInRow r) | r <- (spyGrid sb)]
        cardsInRow row = [vLimit 30 $ hLimit 25 $ (drawSpyCard pcard currCursor) | pcard <- row]


renderHint :: String -> Int -> Widget Name
renderHint hintW hintNumW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ (str (hintW ++ "," ++ (show hintNumW)))

getRedTeamScoreBoard :: Int -> Widget Name
getRedTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleRedCell $ str ("Team Red score :" ++ (show score))

getBlueTeamScoreBoard :: Int -> Widget Name
getBlueTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleBlueCell $ str ("Team Blue score :" ++ (show score))

renderTurn :: Bool -> CardColor -> Widget Name
renderTurn False playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Teams's Turn")
renderTurn True playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Spy's Turn")

drawPlayerStats :: SpyGameState -> Widget Name
drawPlayerStats sb = ((getBlueTeamScoreBoard (sBlueTeamScore sb)) <=> (getRedTeamScoreBoard (sRedTeamScore sb))) <+> ((padLeft Max (renderHint hintWord hintNum)) <=> (padLeft Max (renderTurn (sSpyMastersTurn sb) (sTeamTurn sb))))
    where SHint hintWord hintNum = sSpyHint sb

drawSpyBoard :: SpyGameState -> [Widget Name]
drawSpyBoard sb = [(drawGrid sb)<=> (drawPlayerStats sb)]


-- g :: SpyGrid
-- g = [[SCell (Loc 0 0) "COLD" True Red,SCell (Loc 0 1) "DEATH" True Red,SCell (Loc 0 2) "DIAMOND" True Red,SCell (Loc 0 3) "DOG" False Red,SCell (Loc 0 4) "DRESS" False Red],[SCell (Loc 1 0) "FRANCE" False Red,SCell (Loc 1 1) "FIRE" False Red,SCell (Loc 1 2) "GLOVE" False Red,SCell (Loc 1 3) "GOLD" True Blue,SCell (Loc 1 4) "HAND" False Blue],[SCell (Loc 2 0) "JACK" False Blue,SCell (Loc 2 1) "LONDON" False Blue,SCell (Loc 2 2) "NEW YORK" True Blue,SCell (Loc 2 3) "SNOW" False Blue,SCell (Loc 2 4) "WATCH" False Blue],[SCell (Loc 3 0) "ALASKA" False Blue,SCell (Loc 3 1) "FROG" False Blue,SCell (Loc 3 2) "FROST" False Black,SCell (Loc 3 3) "CHAIN" False Yellow,SCell (Loc 3 4) "CHRISTMAS" False Yellow],[SCell (Loc 4 0) "COMB" False Yellow,SCell (Loc 4 1) "JEWELER" False Yellow,SCell (Loc 4 2) "HAIR" False Yellow,SCell (Loc 4 3) "LOVE" False Yellow,SCell (Loc 4 4) "STORY" False Yellow]]
-- sb1 = PlayBoard 
--         {
--             cursor = (Loc 2 2),
--             plgrid = g
--         }

handleSEvent :: SpyGameState -> BrickEvent Name () -> EventM Name (Next Codenames)
handleSEvent spyGameState(VtyEvent (V.EvKey key [])) =
  continue $ case key of
    V.KUp    -> SpyView (moveCursor UpD spyGameState)
    V.KDown  -> SpyView (moveCursor DownD spyGameState)
    V.KLeft  -> SpyView (moveCursor LeftD spyGameState)
    V.KRight -> SpyView (moveCursor RightD spyGameState)
    V.KEnter -> SpyView (updateGame spyGameState)
    _        -> SpyView spyGameState

-- handleEvent :: SpyBoard -> BrickEvent () e -> EventM () (Next SpyBoard)
-- handleEvent sb (VtyEvent (V.EvKey key [V.MCtrl]))  = 
--     case key of
--         -- Quit
--         V.KChar 'q' -> halt sb
--         _           ->  continue sb



handleSEvent _ _ = undefined