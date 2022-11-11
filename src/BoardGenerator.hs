module BoardGenerator where

import System.IO
import Data.Set hiding (map, filter)
import Prelude

data GameBoard =
    Board Matrix Length Width
   deriving (Eq, Show)

data Coord =
        Loc Row Col
    deriving (Eq, Ord, Show)


type Matrix = [[Int]]
type Length = Int
type Width = Int
type Row = Int
type Col = Int

-- gets the length of a list of Coord
getLength :: [Coord] -> Int
getLength [] = 0
getLength (_:ls) = 1 + getLength ls

-- gets the number for the current grid cell
-- -1 for bomb and number of adjacent bombs otherwise 
getNum :: Row -> Col -> Set Coord -> Int
getNum r c mineSet = if member (Loc r c) mineSet
                    then -1
                    else getAdjBombCount r c mineSet

-- gets adjacent bombs count for the current r c grid cell
getAdjBombCount :: Row -> Col -> Set Coord -> Int
getAdjBombCount r c mineSet = getLength (filter (\(Loc x y) -> member (Loc x y) mineSet) (getAdjIndices r c adjDirs []))

-- gets the 8 adjacent indices given r c grid cell
getAdjIndices :: Row -> Col -> [(Int, Int)] -> [Coord] -> [Coord]
getAdjIndices _ _ [] accLis = accLis
getAdjIndices r c ((dx, dy):adjDirs) accLis =  getAdjIndices r c adjDirs (Loc modr modc:accLis)
                                                where
                                                    modr = r + dx
                                                    modc = c + dy
-- initialize game board given length width and mine set
initBoard :: Length -> Width -> Set Coord -> GameBoard
initBoard length width mineSet = Board (map getRow [0..(length-1)]) length width
                where
                    getRow :: Row -> [Col]
                    getRow r = [getNum r c mineSet | c <- [0..(width-1)]]

-- initial list of mine cells
mines :: [Coord]
mines = [Loc 0 0, Loc 1 1, Loc 2 2]

-- initial set of mine cells
mineSet :: Set Coord
mineSet = fromList mines

-- test game board
mat1 :: GameBoard
mat1 = initBoard 4 4 mineSet

-- utility for adjacent cells
adjDirs :: [(Int, Int)]
adjDirs = [(-1,0), (1,0), (0,-1), (0,1), (-1,-1), (-1,1), (1,-1), (1,1)]


main :: IO ()
main = putStrLn "Hello, world!"