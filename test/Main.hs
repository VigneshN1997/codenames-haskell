module Main where


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
  updateSpyHintTests,
  updateWinnerTests,
  isSpyTurnTests,
  playerWaitTests,
  getCardLocTests
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

updateWinnerTests :: Score -> TestTree
updateWinnerTests sc = testGroup "Tests for updating winner"
  [
  scoreTest (\_ -> updateWinner 7 7 Red Black, (), Blue, 1, "updateWinner1"),
  scoreTest (\_ -> updateWinner 7 7 Blue Black, (), Red, 1, "updateWinner2"),
  scoreTest (\_ -> updateWinner 0 7 Blue Blue, (), Red, 1, "updateWinner3"),
  scoreTest (\_ -> updateWinner 6 0 Blue Blue, (), Blue, 1, "updateWinner4")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

isSpyTurnTests :: Score -> TestTree
isSpyTurnTests sc = testGroup "Tests for updating spy turn"
  [
  scoreTest (\_ -> updateSpyMastersTurn Blue Blue, (), False, 1, "isSpyTurn1"),
  scoreTest (\_ -> updateSpyMastersTurn Red Red, (), False, 1, "isSpyTurn2"),
  scoreTest (\_ -> updateSpyMastersTurn Blue Red, (), True, 1, "isSpyTurn3"),
  scoreTest (\_ -> updateSpyMastersTurn Red Blue, (), True, 1, "isSpyTurn4")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

playerWaitTests :: Score -> TestTree
playerWaitTests sc = testGroup "Tests for updating player wait"
  [
  scoreTest (\_ -> updateWait waitingStr 7 7, (), True, 1, "playerWait1"),
  scoreTest (\_ -> updateWait redWonStr 0 4, (), True, 1, "playerWait2"),
  scoreTest (\_ -> updateWait blueWonStr 3 0, (), True, 1, "playerWait3")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

getCardLocTests :: Score -> TestTree
getCardLocTests sc = testGroup "Tests for getting card at a location"
  [ scoreTest (\_ -> getCard (pg {pPlayerCursor = Loc 0 1}), (), PCell (Loc 0 1) "DEATH" False Red, 1, "getCard1"),
    scoreTest (\_ -> getCard (pg {pPlayerCursor = Loc 1 2}), (), PCell (Loc 1 2) "GLOVE" False Red, 1, "getCard2"),
    scoreTest (\_ -> getCard (pg {pPlayerCursor = Loc 2 3}), (), PCell (Loc 2 3) "SNOW" False Blue, 1, "getCard3"),
    scoreTest (\_ -> getCard (pg {pPlayerCursor = Loc 3 4}), (), PCell (Loc 3 4) "CHRISTMAS" False Yellow, 1, "getCard4"),
    scoreTest (\_ -> getCard (pg {pPlayerCursor = Loc 4 4}), (), PCell (Loc 4 4) "STORY" False Yellow, 1, "getCard5")
    ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

-- inputValidationTests :: Score -> TestTree
-- inputValidationTests sc = testGroup "Tests for updating spy hints"
--   [
--   scoreTest (\_ -> updateSpyMastersTurn Blue Blue, (), False, 1, "isSpyTurn1"),
--   scoreTest (\_ -> updateSpyMastersTurn Red Red, (), False, 1, "isSpyTurn2"),
--   scoreTest (\_ -> updateSpyMastersTurn Blue Red, (), True, 1, "isSpyTurn3"),
--   scoreTest (\_ -> updateSpyMastersTurn Red Blue, (), True, 1, "isSpyTurn4")
--   ]
--   where
--     scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
--     scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

probQCProb :: Score -> TestTree
probQCProb sc = testGroup "QuickCheck Properties"
  [ scoreProp sc ("prop_cursor_in_range"      , QCBoard.prop_cursor_in_range     , 5),
  scoreProp sc ("prob_scores_in_range"      , QCBoard.prob_scores_in_range     , 5)
  ]

colorList :: [String]
colorList = ["Red", "Red", "Red", "Red", "Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Black", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow"]

wordList :: [String]
wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

gridSize :: Int
gridSize = 5

sock :: Socket
sock = undefined

pg :: PlayerGameState
pg = createPlayerState wordList colorList sock
