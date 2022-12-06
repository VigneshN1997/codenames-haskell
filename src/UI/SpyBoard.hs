module UI.SpyBoard (
    drawSpyBoard,
    handleSEvent,
    drawInput) where

import Game
import Codenames
import UI.Styles

import Brick
import Brick.Types

import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BW

import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )


import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as C

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Char8 as CH

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Graphics.Vty (Event)

import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )

import Control.Lens
  ( makeFieldsNoPrefix,
    makeLenses,
    (%~),
    (&),
    (?~),
    (^.),
  )


spyInst = [ "enter :  send hint "
              , "hint,#words : input in text box"
              ]

-- send and recieve message APIs
sendMessFromServer :: Socket -> String -> IO ()
sendMessFromServer sock s = do
--   sock <- openConnection
  sendAll sock $ CH.pack s


-- Server socket connection stuff
openConnection :: IO Socket
openConnection = do
                  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
                  let serveraddr = head addrinfos
                  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                  connect sock (addrAddress serveraddr)
                  return sock


runTCPEchoServerForever :: (Eq a, Num a) => Socket -> a -> IO b
runTCPEchoServerForever sock msgNum = do
  (conn, _)     <- accept sock
  _ <- forkIO (rrLoop conn) -- conn and sock are same
  _ <- forkIO (wLoop conn)
  runTCPEchoServerForever sock $! msgNum + 1


rrLoop :: Socket -> IO ()
rrLoop sock = do
  msg <- recv sock 1024
  let s = C.unpack msg
  print s

wLoop :: Socket -> IO ()
wLoop sock = do
    sendAll sock $ C.pack "dwdwdw"
    print "wLoop: completed writing"


getColorBgStyle :: CardColor -> AttrName
getColorBgStyle Red = styleRedCell
getColorBgStyle Blue = styleBlueCell
getColorBgStyle Black = styleBlackCell
getColorBgStyle Yellow = styleYellowCell

getClickedCursorStyle :: String -> CardColor -> Widget Hint
getClickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

getClickedNormalStyle :: String -> CardColor -> Widget Hint
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

drawSpyCard :: SpyCell -> Coord -> Widget Hint
drawSpyCard (SCell (Loc cardx cardy) word True color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle (word ++ "******") color
                                                                                    else getClickedNormalStyle (word ++ "******") color
drawSpyCard (SCell (Loc cardx cardy) word False color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
                                                                                    then getClickedCursorStyle word color
                                                                                    else getClickedNormalStyle word color


drawGrid :: SpyGameState -> Widget Hint
drawGrid sb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Spy View")
    $ vBox rows
    where
        currCursor = sPlayerCursor sb
        rows = [hBox $ (cardsInRow r) | r <- (spyGrid sb)]
        cardsInRow row = [vLimit 30 $ hLimit 25 $ (drawSpyCard pcard currCursor) | pcard <- row]


renderHint :: CardColor -> String -> Widget Hint

renderHint Blue _ = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr (getColorBgStyle Blue) $ str " Blue Team Won"

renderHint Red _ = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr (getColorBgStyle Red) $ str " Red Team Won"

renderHint _ hintW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ str hintW

getRedTeamScoreBoard :: Int -> Widget Hint
getRedTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleRedCell $ str ("Team Red Cards left :" ++ (show score))

getBlueTeamScoreBoard :: Int -> Widget Hint
getBlueTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleBlueCell $ str ("Team Blue Cards left :" ++ (show score))

renderTurn :: Bool -> CardColor -> Widget Hint
renderTurn False playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Teams's Turn")
renderTurn True playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Spy's Turn")

drawPlayerStats :: SpyGameState -> Widget Hint
drawPlayerStats sb = (getBlueTeamScoreBoard (sBlueTeamScore sb) <=> getRedTeamScoreBoard (sRedTeamScore sb)) <+> (padLeft Max (renderHint (sWinner sb) hintWordCount) <=> padLeft Max (renderTurn (sSpyMastersTurn sb) (sTeamTurn sb)))
    where hintWordCount = sSpyHint sb

drawSpyBoard :: SpyGameState -> [Widget Hint]
drawSpyBoard sb = [(drawGrid sb) <=> (drawPlayerStats sb) <=> drawKeyInstructionsSpy]

-- | Draws the user input space
drawInput :: SpyStateAndForm -> Widget Hint
drawInput g = addBorder (T.pack "Word and Count") (E.renderEditor (txt . T.unlines) True (g ^. wordCount))

handleSEvent :: Codenames -> BrickEvent Hint ConnectionTick -> EventM Hint (Next Codenames)

handleSEvent (SpyView sfb) (AppEvent (ConnectionTick csReceived)) = do
                                case csReceived of
                                    S_Str "end" -> continue $ SpyView SpyStateAndForm { _spyState = (endTurn (_spyState sfb)), _wordCount = (_wordCount sfb)}
                                    S_Str "quit" -> halt (SpyView sfb)
                                    S_Str message ->  continue $ (SpyView SpyStateAndForm { _spyState = (updateGame $ updateSelectedCell message (_spyState sfb)), _wordCount = (_wordCount sfb)})

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
                                    liftIO $ (sendMessFromServer (sSock (_spyState sfb)) (sSpyHint (_spyState sfb)))
                                    continue $ SpyView SpyStateAndForm { _spyState = ((_spyState sfb) {sSpyMastersTurn = False}), _wordCount = (_wordCount sfb)}
    _ -> continue . SpyView =<< (updateGameState sfb ev)
-- handleSEvent (SpyView spyGameState) (VtyEvent (V.EvKey key [])) =
  -- case key of
  --   V.KUp    -> continue $ SpyView (moveCursor UpD spyGameState)
  --   V.KDown  -> continue $ SpyView (moveCursor DownD spyGameState)
  --   V.KLeft  -> continue $ SpyView (moveCursor LeftD spyGameState)
  --   V.KRight -> continue $ SpyView (moveCursor RightD spyGameState)
  --   V.KEnter -> do
  --                   liftIO $ (sendMessFromServer (sSock spyGameState) "SERVER MSG")
  --                   continue $ SpyView (updateGame spyGameState)
  --   _        -> continue $ SpyView spyGameState

-- handleEvent :: SpyBoard -> BrickEvent () e -> EventM () (Next SpyBoard)
-- handleEvent sb (VtyEvent (V.EvKey key [V.MCtrl]))  = 
--     case key of
--         -- Quit
--         V.KChar 'q' -> halt sb
--         _           ->  continue sb

updateGameState :: SpyStateAndForm -> Event -> EventM Hint SpyStateAndForm
updateGameState g ev = do
  gEdited <- handleEventLensed g wordCount E.handleEditorEvent ev
  let oldState = g ^. spyState
      newState = oldState {sSpyHint = T.unpack $ T.unlines $ E.getEditContents $ gEdited ^. wordCount}
  return gEdited {_spyState = newState}


drawKeyInstructionsSpy :: Widget Hint
drawKeyInstructionsSpy = (setAvailableSize (31, 12)) $ (withBorderStyle BS.unicodeBold) $ (B.borderWithLabel (str " Help ")) $  (padLeftRight 1) $ str $  unlines $ spyInst  


-- | Adds a rounded border to a widget with the given label
addBorder :: T.Text -> Widget Hint -> Widget Hint
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)
