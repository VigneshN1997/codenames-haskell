module UI.GameUI 
( 
    localApp
) where

import Codenames
import UI.PlayerBoard
import UI.SpyBoard
import UI.Styles

import Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L


-- | Draws the title over another widget w
drawMenu :: Widget Name -> Widget Name
drawMenu w = (withBorderStyle BS.unicodeRounded . B.borderWithLabel (str "Codenames")) (padBottom Max (C.hCenter w))

-- | Renders a list widget under the logo
drawList :: MenuList -> [Widget Name]
drawList l = [drawMenu listWidget]
  where
    listWidget = L.renderList listDrawElement True l

-- | Draws an element of a list based on if it is selected
listDrawElement :: Bool -> String -> Widget n
listDrawElement selected t = C.hCenter $  (str selected_sign) <+> (str t)
  where
    selected_sign = if selected then "* " else "  "

drawGame :: Codenames -> [Widget Name]
drawGame g = case g of
    MainMenu menu -> drawList menu
    PlayerView pb -> drawPlayerBoard pb
    SpyView sb    -> drawSpyBoard sb



-- | Brick app for handling a codenames game
localApp :: M.App Codenames () Name
localApp =
  M.App
    { M.appDraw = drawGame,
      M.appChooseCursor = const . const Nothing,
      M.appHandleEvent = handleSEvent,
      M.appStartEvent = return,
      M.appAttrMap = const attributes
    }