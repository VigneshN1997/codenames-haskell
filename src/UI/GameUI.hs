module UI.GameUI 
( 
  drawGame
) where

import Codenames
import UI.PlayerBoard
import UI.SpyBoard
import UI.Styles

import Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L


-- | Draws the menu screen to select between player view and spymaster view
drawMenu :: Widget Name -> Widget Name
drawMenu w = (withBorderStyle BS.unicodeRounded . B.borderWithLabel (str "Codenames")) (padBottom Max (C.hCenter w))

-- | Draw the list of options
drawList :: MenuList -> [Widget Name]
drawList l = [drawMenu listWidget]
  where
    listWidget = L.renderList listDrawElement True l

-- | Draws an element of a list based on if it is selected
listDrawElement :: Bool -> String -> Widget n
listDrawElement selected t = C.hCenter $  (str selected_sign) <+> (str t)
  where
    selected_sign = if selected then "* " else "  "

-- | The main draw function to draw different screens in the game
drawGame :: Codenames -> [Widget Name]
drawGame g = case g of
    -- MainMenu menu -> drawList menu
    PlayerView pb -> drawPlayerBoard pb
    SpyView sfb    -> drawSpyBoard sb <=> drawInput sfb
      where sb = sfb ^. spyState
-- | Handle main menu key events
-- handleKeyMainMenu :: MenuList -> BrickEvent Name () -> EventM Name (Next Codenames)
-- handleKeyMainMenu l (VtyEvent e) = case e of
--   V.EvKey V.KEsc [] -> exit
--   V.EvKey V.KEnter []
--     | Just i <- L.listSelected l -> case i of
--       0 -> M.continue initialGamePlayer
--       1 -> M.continue initialGameSpy
--       _ -> M.continue getMainMenu
--   ev -> M.continue . MainMenu =<< L.handleListEvent ev l
--   where
--     exit = M.halt $ MainMenu l
-- handleKeyMainMenu l _ = M.continue $ MainMenu l

-- -- | The main handle event function which passes on the event to different handles based on screen
-- handleEventFirst :: Codenames -> BrickEvent Name () -> EventM Name (Next Codenames)
-- handleEventFirst gs eventKey = case gs of
--   MainMenu menuOp -> handleKeyMainMenu menuOp eventKey
--   PlayerView playerGame -> handleKeyPlayer playerGame eventKey
--   SpyView spyGame -> handleSEvent spyGame eventKey


-- -- | Brick app for handling a codenames game
-- localApp :: M.App Codenames () Name
-- localApp =
--   M.App
--     { M.appDraw = drawGame,
--       M.appChooseCursor = const . const Nothing,
--       M.appHandleEvent = handleEventFirst,
--       M.appStartEvent = return,
--       M.appAttrMap = const attributes
--     }