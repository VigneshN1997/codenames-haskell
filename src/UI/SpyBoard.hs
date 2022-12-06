module UI.SpyBoard (
    drawSpyBoard,
    handleSEvent,
    drawInput) where

import Brick
import System.IO()
import Graphics.Vty (Event)
import Control.Lens((^.))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BW
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T

import Game
import Codenames
import UI.Styles
import Common

spyInst :: [[Char]]
spyInst = [ "enter :  send hint "
              , "hint,#words : input in text box"
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

-- | Render a clicked card on which player cursor is not present
getClickedNormalStyle :: String -> CardColor -> Widget Hint
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

-- | Render a single card
drawSpyCard :: SpyCell -> Coord -> Widget Hint
drawSpyCard (SCell (Loc cardx cardy) word True color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle (word ++ "******") color
                                                                                    else getClickedNormalStyle (word ++ "******") color
drawSpyCard (SCell (Loc cardx cardy) word False color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle word color
                                                                                    else getClickedNormalStyle word color

-- | Render the spy side game board
drawGrid :: SpyGameState -> Widget Hint
drawGrid sb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Spy View")
    $ vBox rows
    where
        currCursor = sPlayerCursor sb
        rows = [hBox $ (cardsInRow r) | r <- (spyGrid sb)]
        cardsInRow row = [vLimit 30 $ hLimit 25 $ (drawSpyCard pcard currCursor) | pcard <- row]

-- | Render hint on the spymaster view
renderHint :: CardColor -> String -> Widget Hint
renderHint Blue _ = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr (getColorBgStyle Blue) $ str " Blue Team Won"
renderHint Red _ = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr (getColorBgStyle Red) $ str " Red Team Won"
renderHint _ hintW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ str hintW

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
drawPlayerStats :: SpyGameState -> Widget Hint
drawPlayerStats sb = (getBlueTeamScoreBoard (sBlueTeamScore sb) <=> getRedTeamScoreBoard (sRedTeamScore sb)) <+> (padLeft Max (renderHint (sWinner sb) hintWordCount) <=> padLeft Max (renderTurn (sSpyMastersTurn sb) (sTeamTurn sb)))
    where hintWordCount = sSpyHint sb

-- | Render the spy side view
drawSpyBoard :: SpyGameState -> [Widget Hint]
drawSpyBoard sb = [(drawGrid sb) <=> (drawPlayerStats sb) <=> drawKeyInstructionsSpy]

-- | Draws the user input space
drawInput :: SpyStateAndForm -> Widget Hint
drawInput g = addBorder (T.pack "Word and Count") (E.renderEditor (txt . T.unlines) True (g ^. wordCount))

-- | Get the text from the text editor and update the spy hint
updateHintOnUI :: SpyStateAndForm -> Event -> EventM Hint SpyStateAndForm
updateHintOnUI g ev = do
  gEdited <- handleEventLensed g wordCount E.handleEditorEvent ev
  let oldState = g ^. spyState
      newState = oldState {sSpyHint = T.unpack $ T.unlines $ E.getEditContents $ gEdited ^. wordCount}
  return gEdited {_spyState = newState}


drawKeyInstructionsSpy :: Widget Hint
drawKeyInstructionsSpy = (setAvailableSize (31, 12)) $ (withBorderStyle BS.unicodeBold) $ (B.borderWithLabel (str " Help ")) $  (padLeftRight 1) $ str $  unlines $ spyInst  


-- | Adds a rounded border to a widget with the given label
addBorder :: T.Text -> Widget Hint -> Widget Hint
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

-- | Handle brick app events (from brick channel)
handleSEvent :: Codenames -> BrickEvent Hint ConnectionTick -> EventM Hint (Next Codenames)
handleSEvent (SpyView sfb) (AppEvent (ConnectionTick csReceived)) = do
                                case csReceived of
                                    S_Str "end" -> continue $ SpyView SpyStateAndForm { _spyState = (endTurn (_spyState sfb)), _wordCount = (_wordCount sfb)}
                                    S_Str "quit" -> halt (SpyView sfb)
                                    S_Str message ->  continue $ (SpyView SpyStateAndForm { _spyState = (updateGame $ updateSelectedCell message (_spyState sfb)), _wordCount = (_wordCount sfb)})

-- | Handle key events on the spy view
handleSEvent (SpyView sfb) (VtyEvent ev) =
  case ev of
    V.EvKey V.KEnter [] -> 
                            if (sSpyHint (_spyState sfb) == redWonStr) || (sSpyHint (_spyState sfb) == blueWonStr)
                              then (continue $ SpyView sfb)
                              else 
                                if (invalidHint (sSpyHint (_spyState sfb)))
                                then  continue $ SpyView SpyStateAndForm { _spyState = ((_spyState sfb) {sSpyHint = "ENTER VALID HINT!!"}), _wordCount = (_wordCount sfb)}
                                else
                                  do
                                    liftIO $ (sendMess (sSock (_spyState sfb)) (sSpyHint (_spyState sfb)))
                                    continue $ SpyView SpyStateAndForm { _spyState = ((_spyState sfb) {sSpyMastersTurn = False}), _wordCount = (_wordCount sfb)}
    _ -> continue . SpyView =<< (updateHintOnUI sfb ev)

handleSEvent _ _ = undefined