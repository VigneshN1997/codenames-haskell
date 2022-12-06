module UI.GameUI 
( 
  drawGame
) where

import Brick
import Control.Lens((^.))

import Game
import Codenames
import UI.PlayerBoard
import UI.SpyBoard

-- | The main draw function to draw different screens in the game
drawGame :: Codenames -> [Widget Hint]
drawGame g = case g of
    PlayerView pb -> drawPlayerBoard pb
    SpyView sfb    -> [(vBox $ drawSpyBoard sb) <=> drawInput sfb]
      where sb = sfb ^. spyState