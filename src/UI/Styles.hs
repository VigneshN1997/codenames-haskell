module UI.Styles (
    stylePlayerCursor, 
    styleRedCell, 
    styleBlueCell, 
    styleYellowCell, 
    styleBlackCell, 
    styleUnclickedCell, 
    styleBoard,
    cursorBorderStyle,
    attributes
) where

import Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Brick.Widgets.Core as BW

stylePlayerCursor, styleRedCell, styleBlueCell, styleYellowCell, styleBlackCell, styleUnclickedCell, styleBoard :: AttrName
stylePlayerCursor    = attrName "stylePlayerCursor"
styleRedCell = attrName "styleRedCell"
styleBlueCell = attrName "styleBlueCell"
styleYellowCell = attrName "styleYellowCell"
styleBlackCell = attrName "styleBlackCell"
styleUnclickedCell = attrName "styleUnclickedCell"
styleBoard = attrName "styleBoard"


cursorBorderStyle :: BS.BorderStyle
cursorBorderStyle =
    BS.BorderStyle { BS.bsCornerTL = '/'
                   , BS.bsCornerTR = '\\'
                   , BS.bsCornerBR = '/'
                   , BS.bsCornerBL = '\\'
                   , BS.bsIntersectFull = '.'
                   , BS.bsIntersectL = '.'
                   , BS.bsIntersectR = '.'
                   , BS.bsIntersectT = '.'
                   , BS.bsIntersectB = '.'
                   , BS.bsHorizontal = '*'
                   , BS.bsVertical = '!'
                   }

attributes :: AttrMap
attributes = attrMap (V.defAttr)
  [ (stylePlayerCursor, fg V.green)
  , (styleRedCell , bg V.red)
  , (styleBlueCell , bg V.blue)
  , (styleYellowCell, bg V.magenta)
  , (styleBlackCell, bg V.brightBlack)
  , (styleUnclickedCell, V.defAttr)
  , (styleBoard, V.defAttr)
  ]