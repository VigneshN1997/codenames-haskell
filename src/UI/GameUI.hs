module UI.GameUI 
( 
    localApp
) where

import Codenames
import UI.PlayerBoard
import UI.Styles

import Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Brick.Widgets.Core as BW

drawGame :: Codenames -> [Widget ()]
drawGame g = case g of
  PlayerView pb -> drawPlayerBoard pb



-- | Brick app for handling a codenames game
localApp :: M.App Codenames e ()
localApp =
  M.App
    { M.appDraw = drawGame,
      M.appChooseCursor = const . const Nothing,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return,
      M.appAttrMap = const attributes
    }