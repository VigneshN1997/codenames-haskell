module Main (main) where

import Game

import Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U

type Name = String


stylePlayerCursor, styleRedCell, styleBlueCell, styleYellowCell, styleBlackCell, styleUnclickedCell, styleBoard :: AttrName
stylePlayerCursor    = attrName "stylePlayerCursor"
styleRedCell = attrName "styleRedCell"
styleBlueCell = attrName "styleBlueCell"
styleYellowCell = attrName "styleYellowCell"
styleBlackCell = attrName "styleBlackCell"
styleUnclickedCell = attrName "styleUnclickedCell"
styleBoard = attrName "styleBoard"

attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (stylePlayerCursor    , bg V.green)
  , (styleRedCell , fg V.red)
  , (styleBlueCell , fg V.blue)
  , (styleYellowCell, fg V.yellow)
  , (styleBlackCell, fg V.brightBlack)
  , (styleUnclickedCell, V.defAttr)
  , (styleBoard, fg V.magenta)
  ]


drawGrid :: PlayerBoard -> Widget ()
drawGrid pb = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel ((withAttr styleBoard) $ str "Codenames Player View")
    $ vBox rows
    where
        rows = [hBox $ (cardsInRow r) | r <- (plgrid pb)]
        cardsInRow row = [hLimit 20 $ withBorderStyle BS.unicodeBold $ B.border $ C.hCenter $ padAll 1 $ (withAttr styleUnclickedCell $ str w) | (PCell _ w _ _) <- row]

drawUI :: PlayerBoard -> Widget ()
drawUI pb = drawGrid pb

downloadedColorList :: [CardColor]
downloadedColorList = [Red, Red, Red, Red, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Black, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]
wordList :: [String]
wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

pb :: PlayerBoard
pb = createPlayerGrid wordList downloadedColorList


main :: IO ()
main = M.simpleMain (drawUI pb)
