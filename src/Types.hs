

module Types where

import Data.Set hiding (map, filter)
import Prelude
import Data.Function ((&))
import Lens.Micro (ix, (%~))

data Coord =
        Loc RIdx CIdx
    deriving (Eq, Ord, Show)

data Cell = 
    Mine Coord Flag Clicked
    | Normal Coord Int Flag Clicked
    deriving (Eq, Show)

type Row = [Cell]

type Grid = [Row]

type RIdx = Int
type CIdx = Int

type Flag = Bool
type Clicked = Bool

data GameBoard = Board
  { cursor :: Coord
  , grid :: Grid
  , nrows :: Int
  , ncols :: Int
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

moveCursor :: Direction -> GameBoard -> GameBoard
moveCursor direction game = game {cursor = cur}
                                where
                                    (Loc r c) = cursor game
                                    nr = nrows game
                                    nc = ncols game
                                    cur = case direction of
                                            UpD -> Loc (wrapAroundCursor (r-1) nr) c
                                            DownD -> Loc (wrapAroundCursor (r+1) nr) c
                                            LeftD -> Loc r (wrapAroundCursor (c-1) nc)
                                            RightD -> Loc r (wrapAroundCursor (c+1) nc)

updateCell :: (Cell -> Cell) -> GameBoard -> GameBoard
updateCell updateFn game = game { grid = grid game & ix y . ix x %~ updateFn }
  where (Loc x y) = cursor game


flagCell :: GameBoard -> GameBoard
flagCell game = updateCell updateFn game
    where
        updateFn :: Cell -> Cell
        updateFn cell = case cell of
                            Normal coord count flag False -> Normal coord count (not flag) False
                            Normal coord count _ True -> Normal coord count False True
                            Mine coord flag False -> Mine coord (not flag) False
                            Mine coord _ True -> Mine coord False True

adjDirs :: [(Int, Int)]
adjDirs = [(-1,0), (1,0), (0,-1), (0,1), (-1,-1), (-1,1), (1,-1), (1,1)]

getLength :: [Coord] -> Int
getLength [] = 0
getLength (_:ls) = 1 + getLength ls

getCell :: RIdx -> CIdx -> Set Coord -> Cell
getCell r c mineSet = if (member (Loc r c) mineSet)
                    then (Mine (Loc r c) False False)
                    else (Normal (Loc r c)  (getAdjBombCount r c mineSet) False False)

getAdjBombCount :: RIdx -> CIdx -> Set Coord -> Int
getAdjBombCount r c mineSet = getLength (filter (\(Loc x y) -> (member (Loc x y) mineSet)) (getAdjIndices r c adjDirs []))


getAdjIndices :: RIdx -> CIdx -> [(Int, Int)] -> [Coord] -> [Coord]
getAdjIndices _ _ [] accLis = accLis
getAdjIndices r c ((dx, dy):dirs) accLis =  getAdjIndices r c dirs ((Loc modr modc):accLis)
                                                where
                                                    modr = r + dx
                                                    modc = c + dy
-- initialize game board
initBoard :: Int -> Int -> Set Coord -> GameBoard
initBoard nr nc mineSet = 
                Board {
                        cursor = (Loc 0 0),
                        grid = (map getRow [0..(nr-1)]),
                        nrows = nr,
                        ncols = nc
                    } 
                where
                    getRow :: RIdx -> [Cell]
                    getRow r = [getCell r c mineSet | c <- [0..(nc-1)]]


-- mines = [(Loc 0 0), (Loc 1 1), (Loc 2 2)]
-- mineSet = fromList mines
