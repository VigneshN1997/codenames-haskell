module UI.PlayerBoard (
    drawPlayerBoard,
    handleEvent
) where

import Game
import Codenames
import UI.Styles

import Control.Monad (void)
import Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Brick.Widgets.Core as BW

type Name = String




getColorBgStyle :: CardColor -> AttrName
getColorBgStyle Red = styleRedCell
getColorBgStyle Blue = styleBlueCell
getColorBgStyle Black = styleBlackCell
getColorBgStyle Yellow = styleYellowCell

getClickedCursorStyle :: String -> CardColor -> Widget ()
getClickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

getUnclickedCursorStyle :: String -> CardColor -> Widget ()
getUnclickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

getClickedNormalStyle :: String -> CardColor -> Widget ()
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word


getUnclickedNormalStyle ::  String -> CardColor -> Widget ()
getUnclickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

drawPlayerCard :: PlayerCell -> Coord -> Widget ()
drawPlayerCard (PCell (Loc cardx cardy) word True color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle word color
                                                                                    else getClickedNormalStyle word color
                                                                                        
drawPlayerCard (PCell (Loc cardx cardy) word False color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getUnclickedCursorStyle word color
                                                                                    else getUnclickedNormalStyle word color

drawGrid :: PlayerGameState -> Widget ()
drawGrid pb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Player View")
    $ vBox rows
    where
        currCursor = playerCursor pb
        rows = [hBox $ (cardsInRow r) | r <- (playerGrid pb)]
        cardsInRow row = [vLimit 30 $ hLimit 25 $ (drawPlayerCard pcard currCursor) | pcard <- row]


renderHint :: String -> Int -> Widget ()
renderHint hintW hintNumW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ (str (hintW ++ "," ++ (show hintNumW)))

getRedTeamScoreBoard :: Int -> Widget ()
getRedTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleRedCell $ str ("Team Red score :" ++ (show score))

getBlueTeamScoreBoard :: Int -> Widget ()
getBlueTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleBlueCell $ str ("Team Blue score :" ++ (show score))

renderPlayerTurn :: CardColor -> Widget ()
renderPlayerTurn playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Team's Turn")

drawPlayerStats :: PlayerGameState -> Widget ()
drawPlayerStats pb = ((getBlueTeamScoreBoard (blueTeamScore pb)) <=> (getRedTeamScoreBoard (redTeamScore pb))) <+> ((padLeft Max (renderHint hintWord hintNum)) <=> (padLeft Max (renderPlayerTurn (teamTurn pb))))
    where SHint hintWord hintNum = spyHint pb

drawPlayerBoard :: PlayerGameState -> [Widget ()]
drawPlayerBoard pb = [(drawGrid pb) <=> (drawPlayerStats pb)]


-- g :: PlayerGrid
-- g = [[PCell (Loc 0 0) "COLD" True Red,PCell (Loc 0 1) "DEATH" True Red,PCell (Loc 0 2) "DIAMOND" True Red,PCell (Loc 0 3) "DOG" False Red,PCell (Loc 0 4) "DRESS" False Red],[PCell (Loc 1 0) "FRANCE" False Red,PCell (Loc 1 1) "FIRE" False Red,PCell (Loc 1 2) "GLOVE" False Red,PCell (Loc 1 3) "GOLD" True Blue,PCell (Loc 1 4) "HAND" False Blue],[PCell (Loc 2 0) "JACK" False Blue,PCell (Loc 2 1) "LONDON" False Blue,PCell (Loc 2 2) "NEW YORK" True Blue,PCell (Loc 2 3) "SNOW" False Blue,PCell (Loc 2 4) "WATCH" False Blue],[PCell (Loc 3 0) "ALASKA" False Blue,PCell (Loc 3 1) "FROG" False Blue,PCell (Loc 3 2) "FROST" False Black,PCell (Loc 3 3) "CHAIN" False Yellow,PCell (Loc 3 4) "CHRISTMAS" False Yellow],[PCell (Loc 4 0) "COMB" False Yellow,PCell (Loc 4 1) "JEWELER" False Yellow,PCell (Loc 4 2) "HAIR" False Yellow,PCell (Loc 4 3) "LOVE" False Yellow,PCell (Loc 4 4) "STORY" False Yellow]]
-- pb1 = PlayBoard 
--         {
--             cursor = (Loc 2 2),
--             plgrid = g
--         }

handleEvent (PlayerView pb) (VtyEvent (V.EvKey key [])) =
  continue $ case key of
    V.KUp    -> PlayerView (moveCursor UpD pb)
    V.KDown  -> PlayerView (moveCursor DownD pb)
    V.KLeft  -> PlayerView (moveCursor LeftD pb)
    V.KRight -> PlayerView (moveCursor RightD pb)
    V.KEnter -> PlayerView (updateCurrentTurnsAndScore (selectCard pb))
    _        -> PlayerView pb

-- handleEvent :: PlayerBoard -> BrickEvent () e -> EventM () (Next PlayerBoard)
-- handleEvent pb (VtyEvent (V.EvKey key [V.MCtrl]))  = 
--     case key of
--         -- Quit
--         V.KChar 'q' -> halt pb
--         _           ->  continue pb



handleEvent pb _ = undefined

data Tick = Int



-- app :: App PlayerBoard e ()
-- app = App { appDraw = drawUI
--           , appChooseCursor = const . const Nothing
--           , appHandleEvent = handleEvent
--           , appStartEvent = return
--           , appAttrMap = const attributes
--           }