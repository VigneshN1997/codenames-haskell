{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Test where

-- import Test.Tasty
-- import Common
-- import Prelude hiding (maximum)
-- -- import System.FilePath ((</>))
-- import Game

-- main :: IO ()
-- main = runTests 
--   [ probParse
--   ]

-- probParse ::  Score -> TestTree
-- probParse sc = testGroup "Problem 3: Parse"
--   [ scoreTest ((\_ -> 1 + 2), (), 3, 5, "basic-test")
--   ]
--   where
--     scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
--     scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)





-- colorList = ["Red", "Red", "Red", "Red", "Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Blue", "Black", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow", "Yellow"]
-- wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]
-- gridSize = 5
-- tupleList = zip3 wordList (map convertToColor colorList) [0..((gridSize*gridSize) - 1)]
-- cellList = map createPlayerCard tupleList
-- pb = createBoard cellList


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
--                         pWinner = Yellow
--                     }
-- getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis