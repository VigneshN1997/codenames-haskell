module Game 
( PlayerCell(..)
  , PlayerRow
  , PlayerGrid
  , PlayerBoard(..)
  , Direction(..)
  , CardColor(..)
  , Coord(..)
  , createPlayerGrid
  , moveCursor
  , selectCard

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


-- data TeamDetails

data PlayerCell = PCell Coord String Clicked CardColor
    deriving (Eq, Show)
type PlayerRow = [PlayerCell]
type PlayerGrid = [PlayerRow]

data PlayerBoard = PlayBoard
                    {
                        cursor :: Coord,
                        plgrid :: PlayerGrid,
                        pl1score :: Int,
                        pl2score :: Int 
                    } deriving (Show)

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

moveCursor :: Direction -> PlayerBoard -> PlayerBoard
moveCursor direction game = game {cursor = cur}
                                where
                                    (Loc r c) = cursor game
                                    cur = case direction of
                                            UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
                                            DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
                                            LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
                                            RightD -> Loc r (wrapAroundCursor (c+1) gridSize)


updateCard :: (PlayerCell -> PlayerCell) -> PlayerBoard -> PlayerBoard
updateCard updateFn pb = pb { plgrid = plgrid pb & ix x . ix y %~ updateFn }
  where (Loc x y) = cursor pb


selectCard :: PlayerBoard -> PlayerBoard
selectCard pb = updateCard updateFn pb
    where
        updateFn :: PlayerCell -> PlayerCell
        updateFn cell = case cell of
                            PCell l w True c -> PCell l w True c
                            PCell l w False c -> PCell l w True c


-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


createPlayerCard :: (String, CardColor, Idx) -> PlayerCell
createPlayerCard (word, color, idx) = PCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) word False color

createSpyCard :: (CardColor, Idx) -> SpyCell
createSpyCard (color, idx) = SCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) False color

createPlayerGrid :: [String] -> [CardColor] -> PlayerBoard
createPlayerGrid wordlis colors = createBoard cellList
        where
            tupleList = zip3 wordlis colors [0..((gridSize*gridSize) - 1)]
            cellList = map createPlayerCard tupleList
            createBoard :: [PlayerCell] -> PlayerBoard 
            createBoard cls = PlayBoard 
                                {
                                    cursor = (Loc 0 0),
                                    plgrid = map (getSlice cls) [0..(gridSize - 1)],
                                    pl1score = 0,
                                    pl2score = 0
                                }
            getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis


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


downloadedColorList :: [CardColor]
downloadedColorList = [Red, Red, Red, Red, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Black, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]
wordList :: [String]
wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

gridSize::Int
gridSize = 5

