module QCBoard(
      prop_cursor_in_range
    , prob_scores_in_range
) where

import Test.QuickCheck 
import Network.Socket
import Game

data PlayerOp 
  = MoveOp Direction
  | UpdateOp
  deriving (Show)

dirs :: [Direction]
dirs = [UpD, DownD, LeftD, RightD]

genMoves :: Gen (PlayerOp)
genMoves = do
  move <- elements dirs
  return (MoveOp move)

genUpdate :: Gen (PlayerOp)
genUpdate = return UpdateOp


genGameOp ::  Gen (PlayerOp)
genGameOp  = frequency [(4, genMoves), (2, genUpdate)]


simulateOps ::  [PlayerOp] -> PlayerGameState
simulateOps ops            = foldr doOp pgame ops
  where 
      doOp (MoveOp dir) pg = moveCursor dir pg
      doOp (UpdateOp) pg   = updateGame pg

inRangeCursor :: PlayerGameState -> Bool
inRangeCursor pg = if (x >= 0 && x < gridSize) && (y >= 0 && y < gridSize)
                        then True
                        else False
                    where
                        Loc x y = pPlayerCursor pg

inRangeScores :: PlayerGameState -> Bool
inRangeScores pg = if (redScore >= 0 && redScore <= 9) && (blueScore >= 0 && blueScore <= 8)
                        then True
                        else False
                    where
                        redScore = pRedTeamScore pg
                        blueScore = pBlueTeamScore pg

prop_cursor_in_range :: Property
prop_cursor_in_range = forAll (listOf genMoves) $ \ops ->
                    inRangeCursor (simulateOps ops)

prob_scores_in_range :: Property
prob_scores_in_range = forAll (listOf genGameOp) $ \ops ->
                    inRangeScores (simulateOps ops)


-- tupleList = zip3 wordList (map convertToColor colorList) [0..((gridSize*gridSize) - 1)]
-- cellList = map createPlayerCard tupleList

-- pb :: PlayerGameState
-- pb = createBoard cellList

colorList = ["Red", "Red", "Red", "Red", "Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Black", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow"]
wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]
gridSize = 5

sock :: Socket
sock = undefined

pgame :: PlayerGameState
pgame = createPlayerState colorList wordList sock