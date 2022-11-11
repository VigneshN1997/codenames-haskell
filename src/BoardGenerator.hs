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


getLength :: [Coord] -> Int
getLength [] = 0
getLength (_:ls) = 1 + getLength ls

getNum :: Row -> Col -> Set Coord -> Int
getNum r c mineSet = if (member (Loc r c) mineSet)
                    then -1
                    else (getAdjBombCount r c mineSet)

getAdjBombCount :: Row -> Col -> Set Coord -> Int
getAdjBombCount r c mineSet = getLength (filter (\(Loc x y) -> (member (Loc x y) mineSet)) (getAdjIndices r c adjDirs []))


getAdjIndices :: Row -> Col -> [(Int, Int)] -> [Coord] -> [Coord]
getAdjIndices r c [] accLis = accLis
getAdjIndices r c ((dx, dy):adjDirs) accLis =  getAdjIndices r c adjDirs ((Loc modr modc):accLis)
                                                where
                                                    modr = r + dx
                                                    modc = c + dy
-- initialize game board
initBoard :: Length -> Width -> Set Coord -> GameBoard
initBoard length width mineSet = Board (map (getRow) [0..(length-1)]) length width
                where
                    getRow :: Row -> [Col]
                    getRow r = [getNum r c mineSet | c <- [0..(width-1)]]




-- filter (\(Loc x y) -> (member (Loc x y) mineSet)) (

mines = [(Loc 0 0), (Loc 1 1), (Loc 2 2)]
mineSet = fromList mines

mat1 = initBoard 4 4 mineSet
adjDirs = [(-1,0), (1,0), (0,-1), (0,1), (-1,-1), (-1,1), (1,-1), (1,1)]


main :: IO ()
main = putStrLn "Hello, world!"