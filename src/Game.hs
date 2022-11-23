module Game 
( PlayerCell(..)
  , PlayerRow
  , PlayerGrid
--   , PlayerBoard(..)
  , PlayerGameState(..)
  , Direction(..)
  , CardColor(..)
  , Coord(..)
  , SpyHint(..)
  , createPlayerGrid
  , moveCursor
  , selectCard
  , updateCurrentTurnsAndScore,
  updatePlayerGame
--   , colorPlayerCell

) where

import Lens.Micro (ix, (%~), (&))


type RIdx = Int
type CIdx = Int
type Idx = Int
type Clicked = Bool
type WCount = Int

data CardColor = Red | Blue | Black | Yellow
    deriving (Eq, Show)

data Coord =
        Loc RIdx CIdx
    deriving (Eq, Ord, Show)

data SpyCell = SCell Coord Clicked CardColor
    deriving (Eq, Show)

type SpyRow = [SpyCell]
type SpyGrid = [SpyRow]

data SpymasterBoard = SpyBoard
                        {
                            cursorClicked :: Coord,
                            spgrid :: SpyGrid
                        } deriving (Show)

data SpyHint = SHint String WCount
                deriving (Show)


-- data TeamDetails

data PlayerCell = PCell Coord String Clicked CardColor
    deriving (Eq, Show)
type PlayerRow = [PlayerCell]
type PlayerGrid = [PlayerRow]

-- data PlayerBoard = PlayBoard
--                     {
--                         cursor :: Coord,
--                         plgrid :: PlayerGrid,
--                         pl1score :: Int,
--                         pl2score :: Int 
--                     } deriving (Show)

data Direction
  = UpD
  | DownD
  | LeftD
  | RightD
  deriving (Show)

wrapAroundCursor :: Int -> Int -> Int
wrapAroundCursor val n
    | val >= n = val - n
    | val < 0 = val + n
    | otherwise = val

-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

downloadedColorList :: [CardColor]
downloadedColorList = [Red, Red, Red, Red, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Black, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]
egwordList :: [String]
egwordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

gridSize::Int
gridSize = 5

-- //////////////Player Game State/////////////////

data PlayerGameState = PlayerGameState {
    playerGrid    :: PlayerGrid,
    wordList      :: [String],
    cardColor     :: [CardColor],
    redTeamScore  :: Int,
    blueTeamScore :: Int,
    teamTurn      :: CardColor,
    spyHint       :: SpyHint,
    playerCursor  :: Coord,
    spyMastersTurn :: Bool
} deriving (Show)

-- utility functions
-- updatePlayerCell :: (PlayerCell -> PlayerCell) -> PlayerGameState -> PlayerGameState
-- updatePlayerCell updatePcellFn game = game {playerGrid = playerGrid game & ix y . ix x %~ updatePcellFn }
--   where (Loc x y) = playerCursor game

createPlayerCard :: (String, CardColor, Idx) -> PlayerCell
createPlayerCard (word, color, idx) = PCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) word False color

createSpyCard :: (CardColor, Idx) -> SpyCell
createSpyCard (color, idx) = SCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) False color

createPlayerGrid :: [String] -> [CardColor] -> PlayerGameState
createPlayerGrid wordlis colors = createBoard cellList
        where
            tupleList = zip3 wordlis colors [0..((gridSize*gridSize) - 1)]
            cellList = map createPlayerCard tupleList
            createBoard :: [PlayerCell] -> PlayerGameState 
            createBoard cls = PlayerGameState 
                                {
                                    playerCursor = (Loc 0 0),
                                    playerGrid = map (getSlice cls) [0..(gridSize - 1)],
                                    redTeamScore = 0,
                                    blueTeamScore = 0,
                                    wordList = egwordList,
                                    cardColor = downloadedColorList,
                                    teamTurn = Red,
                                    spyMastersTurn = False,
                                    spyHint = SHint "Temperature" 2
                                }
            getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis

moveCursor :: Direction -> PlayerGameState -> PlayerGameState
moveCursor direction game = game {playerCursor = cur}
                                where
                                    (Loc r c) = playerCursor game
                                    cur = case direction of
                                            UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
                                            DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
                                            LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
                                            RightD -> Loc r (wrapAroundCursor (c+1) gridSize)


updateCard :: (PlayerCell -> PlayerCell) -> PlayerGameState -> PlayerGameState
updateCard updateFn pb = pb { playerGrid = playerGrid pb & ix x . ix y %~ updateFn }
  where (Loc x y) = playerCursor pb


selectCard :: PlayerGameState -> PlayerGameState
selectCard pb = updateCard updateFn pb
    where
        updateFn :: PlayerCell -> PlayerCell
        updateFn cell = case cell of
                            PCell l w True c -> PCell l w True c
                            PCell l w False c -> PCell l w True c


getCardColor :: PlayerGameState -> CardColor
getCardColor game = cColor
    where (Loc x y) = playerCursor game
          PCell _ _ _ cColor = playerGrid game !! x !! y

isCardClicked :: PlayerCell -> Bool
isCardClicked (PCell _ _ isClicked _) = isClicked

updateTeam :: CardColor -> CardColor -> CardColor
updateTeam Red cColor = if cColor == Red then Red else Blue
updateTeam Blue cColor = if cColor == Blue then Blue else Red

updateRedTeamScore :: CardColor -> Int -> Int
updateRedTeamScore Red score  = score + 1
updateRedTeamScore _ score = score

updateBlueTeamScore :: CardColor -> Int -> Int
updateBlueTeamScore Blue score  = score + 1
updateBlueTeamScore _ score = score

updateSpyMastersTurn :: CardColor -> CardColor -> Bool
updateSpyMastersTurn Red cColor = if cColor == Red then False else True
updateSpyMastersTurn Blue cColor = if cColor == Blue then False else True

-- functions that should run on clicks from the player

-- colorPlayerCell :: PlayerGameState -> PlayerGameState
-- colorPlayerCell game = updatePlayerCell updatePcellFn game
--     where
--         updatePcellFn :: PlayerCell -> PlayerCell
--         updatePcellFn (PCell coord str clicked col) = (PCell coord str True col)

updateCurrentTurnsAndScore :: PlayerGameState -> PlayerGameState
updateCurrentTurnsAndScore game = game {teamTurn = updateTeam (teamColor) (cColor),
                                    redTeamScore = updateRedTeamScore cColor redScore ,
                                    blueTeamScore = updateBlueTeamScore cColor blueScore,
                                    spyMastersTurn = updateSpyMastersTurn (teamColor) cColor
                                    }
                                where cColor = getCardColor game
                                      teamColor = teamTurn game
                                      redScore = redTeamScore game
                                      blueScore = blueTeamScore game
                            

updatePlayerGame:: PlayerGameState -> PlayerGameState
updatePlayerGame game = if (isCardClicked currCard)
                        then game
                        else updateCurrentTurnsAndScore (selectCard game)
                        
                        where
                            (Loc x y) = playerCursor game
                            currCard = playerGrid game !! x !! y

-- functions that should run after new hints from the spy master
updateSpyHint :: PlayerGameState -> PlayerGameState
updateSpyHint game = game


-- //////////////Spy Game State/////////////////

data SpyGameState = SpyGameState {
    spyGrid        :: SpyGrid,
    wordList       :: [String],
    cardColor      :: [CardColor],
    redTeamScore   :: Int,
    blueTeamScore  :: Int,
    teamTurn       :: CardColor,
    spyHint        :: SpyHint,
    spyMastersTurn :: Bool,
    playerCursor   :: Coord
} deriving (Show)


-- utility functions

-- main functions

createSpyGrid :: [CardColor] -> SpymasterBoard
createSpyGrid colors = createBoard cellList
        where
            tupleList = zip colors [0..((gridSize*gridSize) - 1)]
            cellList = map createSpyCard tupleList
            createBoard :: [SpyCell] -> SpymasterBoard 
            createBoard cls = SpyBoard 
                                {
                                    cursorClicked = (Loc 0 0),
                                    spgrid = map (getSlice cls) [0..(gridSize - 1)]
                                }
            getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis

updateSpyHint :: PlayerGameState -> PlayerGameState
updateSpyHint game = game


moveCursor :: Direction -> SpyGameState -> SpyGameState
moveCursor direction game = game {playerCursor = cur}
                                where
                                    (Loc r c) = playerCursor game
                                    cur = case direction of
                                            UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
                                            DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
                                            LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
                                            RightD -> Loc r (wrapAroundCursor (c+1) gridSize)

updateCard :: (SpyCell -> SpyCell) -> SpyGameState -> SpyGameState
updateCard updateFn pb = pb { spyGrid = spyGrid pb & ix x . ix y %~ updateFn }
  where (Loc x y) = playerCursor pb


selectCard :: SpyGameState -> SpyGameState
selectCard sb = updateCard updateFn sb
    where
        updateFn :: SpyCell -> SpyCell
        updateFn cell = case cell of
                            PCell l w True c -> PCell l w True c
                            PCell l w False c -> PCell l w True c


getCardColor :: SpyGameState -> CardColor
getCardColor game = cColor
    where (Loc x y) = playerCursor game
          PCell _ _ _ cColor = spyGrid game !! x !! y

isCardClicked :: SpyCell -> Bool
isCardClicked (SCell _ _ isClicked _) = isClicked

updateTeam :: CardColor -> CardColor -> CardColor
updateTeam Red cColor = if cColor == Red then Red else Blue
updateTeam Blue cColor = if cColor == Blue then Blue else Red

updateRedTeamScore :: CardColor -> Int -> Int
updateRedTeamScore Red score  = score + 1
updateRedTeamScore _ score = score

updateBlueTeamScore :: CardColor -> Int -> Int
updateBlueTeamScore Blue score  = score + 1
updateBlueTeamScore _ score = score

updateSpyMastersTurn :: CardColor -> CardColor -> Bool
updateSpyMastersTurn Red cColor = if cColor == Red then False else True
updateSpyMastersTurn Blue cColor = if cColor == Blue then False else True

-- functions that should run on clicks from the player

-- colorSpyCell :: SpyGameState -> SpyGameState
-- colorSpyCell game = updateSpyCell updatePcellFn game
--     where
--         updatePcellFn :: SpyCell -> SpyCell
--         updatePcellFn (PCell coord str clicked col) = (PCell coord str True col)

updateCurrentTurnsAndScore :: SpyGameState -> SpyGameState
updateCurrentTurnsAndScore game = game {teamTurn = updateTeam (teamColor) (cColor),
                                    redTeamScore = updateRedTeamScore cColor redScore ,
                                    blueTeamScore = updateBlueTeamScore cColor blueScore,
                                    spyMastersTurn = updateSpyMastersTurn (teamColor) cColor
                                    }
                                where cColor = getCardColor game
                                      teamColor = teamTurn game
                                      redScore = redTeamScore game
                                      blueScore = blueTeamScore game
                            

updateSpyGame:: SpyGameState -> SpyGameState
updateSpyGame game = if (isCardClicked currCard)
                        then game
                        else updateCurrentTurnsAndScore (selectCard game)
                        
                        where
                            (Loc x y) = playerCursor game
                            currCard = spyGrid game !! x !! y

-- functions that should run after new hints from the spy master
updateSpyHint :: SpyGameState -> SpyHint -> SpyGameState
updateSpyHint game newHint = game {spyHint = newHint}









        


                   





