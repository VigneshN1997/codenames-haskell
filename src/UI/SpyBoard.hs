module UI.SpyBoard (
    drawSpyBoard,
    handleSEvent,
    drawInput) where

import Game
import Codenames
import UI.Styles

import Brick 
import Brick.Types 
import Foreign.Marshal.Unsafe
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BW
import Lens.Micro.Mtl (use, (.=), zoom)

import Control.Concurrent       
import Control.Monad             
-- import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C

import Network.Socket 
-- import Data.List.Split
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

-- ===============================

getColorBgStyle :: CardColor -> AttrName
getColorBgStyle Red = styleRedCell
getColorBgStyle Blue = styleBlueCell
getColorBgStyle Black = styleBlackCell
getColorBgStyle Yellow = styleYellowCell

getClickedCursorStyle :: String -> CardColor -> Widget Hint
getClickedCursorStyle word color = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word

getUnclickedCursorStyle :: String -> CardColor -> Widget Hint
getUnclickedCursorStyle word _ = withBorderStyle cursorBorderStyle $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

getClickedNormalStyle :: String -> CardColor -> Widget Hint
getClickedNormalStyle word color = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr (getColorBgStyle color) $ BW.padLeftRight 4 $ str word


getUnclickedNormalStyle ::  String -> CardColor -> Widget Hint
getUnclickedNormalStyle word _ = withBorderStyle BS.unicodeBold $ B.border $ C.hCenter  $ withAttr styleUnclickedCell $ str word

drawSpyCard :: SpyCell -> Coord -> Widget Hint
drawSpyCard (SCell (Loc cardx cardy) word _ color) (Loc cursorx cursory) = if (and [(cursorx == cardx), (cursory == cardy)])
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


renderHint :: String -> Int -> Widget Hint
renderHint hintW hintNumW = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleUnclickedCell $ (str (hintW ++ "," ++ (show hintNumW)))

getRedTeamScoreBoard :: Int -> Widget Hint
getRedTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleRedCell $ str ("Team Red score :" ++ (show score))

getBlueTeamScoreBoard :: Int -> Widget Hint
getBlueTeamScoreBoard score = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ withAttr styleBlueCell $ str ("Team Blue score :" ++ (show score))

renderTurn :: Bool -> CardColor -> Widget Hint
renderTurn False playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Teams's Turn")
renderTurn True playerColor = vLimit 10 $ hLimit 30 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ (withAttr (getColorBgStyle playerColor)) $ str ((show playerColor) ++ " Spy's Turn")

drawPlayerStats :: SpyGameState -> Widget Hint
drawPlayerStats sb = ((getBlueTeamScoreBoard (sBlueTeamScore sb)) <=> (getRedTeamScoreBoard (sRedTeamScore sb))) <+> ((padLeft Max (renderHint hintWord 5)) <=> (padLeft Max (renderTurn (sSpyMastersTurn sb) (sTeamTurn sb))))
    where hintWord = sSpyHint sb

drawSpyBoard :: SpyGameState -> [Widget Hint]
drawSpyBoard sb = [(drawGrid sb) <=> (drawPlayerStats sb)]


-- g :: SpyGrid
-- g = [[SCell (Loc 0 0) "COLD" True Red,SCell (Loc 0 1) "DEATH" True Red,SCell (Loc 0 2) "DIAMOND" True Red,SCell (Loc 0 3) "DOG" False Red,SCell (Loc 0 4) "DRESS" False Red],[SCell (Loc 1 0) "FRANCE" False Red,SCell (Loc 1 1) "FIRE" False Red,SCell (Loc 1 2) "GLOVE" False Red,SCell (Loc 1 3) "GOLD" True Blue,SCell (Loc 1 4) "HAND" False Blue],[SCell (Loc 2 0) "JACK" False Blue,SCell (Loc 2 1) "LONDON" False Blue,SCell (Loc 2 2) "NEW YORK" True Blue,SCell (Loc 2 3) "SNOW" False Blue,SCell (Loc 2 4) "WATCH" False Blue],[SCell (Loc 3 0) "ALASKA" False Blue,SCell (Loc 3 1) "FROG" False Blue,SCell (Loc 3 2) "FROST" False Black,SCell (Loc 3 3) "CHAIN" False Yellow,SCell (Loc 3 4) "CHRISTMAS" False Yellow],[SCell (Loc 4 0) "COMB" False Yellow,SCell (Loc 4 1) "JEWELER" False Yellow,SCell (Loc 4 2) "HAIR" False Yellow,SCell (Loc 4 3) "LOVE" False Yellow,SCell (Loc 4 4) "STORY" False Yellow]]
-- sb1 = PlayBoard 
--         {
--             cursor = (Loc 2 2),
--             plgrid = g
--         }


-- mkHintForm :: SpyForm -> Form SpyForm ConnectionTick Hint
-- mkHintForm =
--     let label s w = padBottom (Pad 1) $
--                     (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
--     in newForm [ label "Word And Count" @@=
--                    editTextField wordCount WordCountField (Just 1)
--             --    , label "Count" @@=
--             --        editShowableField count CountField
--                ]

-- | Draws the user input space
drawInput :: SpyStateAndForm -> Widget Hint
drawInput g = addBorder (T.pack "Word and Count") (E.renderEditor (txt . T.unlines) True (g ^. wordCount))

handleSEvent :: Codenames -> BrickEvent Hint ConnectionTick -> EventM Hint (Next Codenames)
handleSEvent (SpyView sfb) (VtyEvent ev) = 
  case ev of
    V.EvKey V.KEnter [] -> continue $ SpyView sfb
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


-- | Adds a rounded border to a widget with the given label
addBorder :: T.Text -> Widget Hint -> Widget Hint
addBorder t = withBorderStyle BS.unicodeRounded . B.borderWithLabel (txt t)

-- handleSEvent (SpyView spyGameState) (AppEvent (ConnectionTick csReceived)) = do
--                                 case csReceived of
--                                     S_Str message ->  continue $ (SpyView (updateHintFromPlayer message spyGameState))


-- -- sock = openConnection
-- handleSEvent (SpyView spyGameState) _ = continue $ SpyView spyGameState