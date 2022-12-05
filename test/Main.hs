module Main where

import System.Exit (exitFailure)

import Test.Tasty
import Common
import Prelude hiding (maximum)
import Network.Socket
-- import System.FilePath ((</>))
import Game
import QCBoard

main :: IO ()
main = runTests
  [ cardTests,
  cursorTests,
  probQCProb,
  endTurnTests,
  updateSpyHintTests
  ]

cardTests ::  Score -> TestTree
cardTests sc = testGroup "Tests for creating game board player cards"
  [ scoreTest ((\_ -> createPlayerCard ("TEST",Red,4) ), (), PCell (Loc 0 4) "TEST" False Red, 1, "card-test1"),
  scoreTest ((\_ -> createPlayerCard ("COL",Blue,5) ), (), PCell (Loc 1 0) "COL" False Blue, 1, "card-test2"),
  scoreTest ((\_ -> createPlayerCard ("COUNTRY",Black,11) ), (), PCell (Loc 2 1) "COUNTRY" False Black, 1, "card-test3"),
  scoreTest ((\_ -> createPlayerCard ("CRICKT",Yellow,17) ), (), PCell (Loc 3 2) "CRICKT" False Yellow, 1, "card-test4"),
  scoreTest ((\_ -> createPlayerCard ("FACE",Blue,23) ), (), PCell (Loc 4 3) "FACE" False Blue, 1, "card-test5")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)


cursorTests ::  Score -> TestTree
cursorTests sc = testGroup "Tests for moving cursor"
  [ scoreTest ((\_ -> pPlayerCursor (moveCursor UpD pg) ), (), Loc 4 0, 1, "move-test1"),
  scoreTest ((\_ -> pPlayerCursor (moveCursor DownD pg) ), (), Loc 1 0, 1, "move-test2"),
  scoreTest ((\_ -> pPlayerCursor (moveCursor RightD pg) ), (), Loc 0 1, 1, "move-test3"),
  scoreTest ((\_ -> pPlayerCursor (moveCursor LeftD pg) ), (), Loc 0 4, 1, "move-test4"),
  scoreTest ((\_ -> pPlayerCursor (moveCursor UpD (moveCursor LeftD pg)) ), (), Loc 4 4, 1, "move-test5")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

endTurnTests :: Score -> TestTree
endTurnTests sc = testGroup "Tests for ending turn of players"
  [
  scoreTest (\_ -> pTeamTurn (endTurn pg), (), Blue, 1, "end-turn1"),
  scoreTest (\_ -> pTeamTurn (endTurn $ endTurn pg), (), Red, 1, "end-turn2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

updateSpyHintTests :: Score -> TestTree
updateSpyHintTests sc = testGroup "Tests for updating spy hints"
  [
  scoreTest (\_ -> pSpyHint (updateSpyHint pg "New Hint"), (), "New Hint", 1, "updateSpyHint1"),
  scoreTest (\_ -> pSpyHint (updateSpyHint pg "Some other hint"), (), "Some other hint", 1, "updateSpyHint2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

probQCProb :: Score -> TestTree
probQCProb sc = testGroup "QuickCheck Properties"
  [ scoreProp sc ("prop_cursor_in_range"      , QCBoard.prop_cursor_in_range     , 5),
  scoreProp sc ("prob_scores_in_range"      , QCBoard.prob_scores_in_range     , 5)
  ]


colorList = ["Red", "Red", "Red", "Red", "Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Black", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow"]
wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]
gridSize = 5
-- tupleList = zip3 wordList (map convertToColor colorList) [0..((gridSize*gridSize) - 1)]
-- cellList = map createPlayerCard tupleList

-- pb :: PlayerGameState
-- pb = createBoard cellList

sock :: Socket
sock = undefined

pg :: PlayerGameState
pg = createPlayerState colorList wordList sock


-- createBoard :: [PlayerCell] -> PlayerGameState 
-- createBoard cls = PlayerGameState 
--                     {
--                         pPlayerCursor = (Loc 0 0),
--                         playerGrid = map (getSlice cls) [0..(gridSize - 1)],
--                         pRedTeamScore = 0,
--                         pBlueTeamScore = 0,
--                         pWordList = wordList,
--                         pCardColor = (map convertToColor colorList),
--                         pTeamTurn = Red,
--                         pSpyMastersTurn = False,
--                         pSpyHint = "Waiting for Hint",
--                         pSock = sock,
--                         pWinner = Yellow
--                     }
-- getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis