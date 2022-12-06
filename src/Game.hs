{-# LANGUAGE TemplateHaskell #-}

module Game 
(Direction(..)
  , CardColor(..)
  , Coord(..)
  , SpyHint(..)
  , PlayerCell(..)
  , SpyCell(..)
  , GameState(..)
  , PlayerGameState(..)
  , SpyGameState(..)
  , PlayerRow
  , PlayerGrid
  , SpyRow
  , SpyGrid
  , createPlayerState
  , createSpyState
  , updateHintFromPlayer
  , updateHintFromSpy
  , convertToColor
  , createPlayerCard
  , slice
  , SpyStateAndForm(..)
  , Hint(..)
  , wordCount
  , spyState
  , updateSelectedCell
  , waitingStr
  , PlayerCell(..)
  , updateWinner
  , redWonStr
  , blueWonStr
  , invalidHint
) where

import Lens.Micro (ix, (%~), (&))
import Network.Socket (Socket)

import Data.Char
import Data.String.Utils

import Lens.Micro (ix, (%~), (&))
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
-- import Control.Lens
-- import Control.Lens.TH
import Lens.Micro.TH

import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as C

import Network.Socket 
import Data.List.Split
import Network.Socket.ByteString (recv, sendAll)
import System.IO(IOMode (ReadMode))
import Control.Monad.IO.Class (MonadIO(liftIO))



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

data SpyCell = SCell Coord String Clicked CardColor
    deriving (Eq, Show)

type SpyRow = [SpyCell]
type SpyGrid = [SpyRow]

data SpymasterBoard = SpyBoard
                        {
                            cursorClicked :: Coord,
                            spgrid :: SpyGrid
                        } deriving (Show)

type SpyHint = String

waitingStr = "Waiting for Hint"
redWonStr = "Red Team Won"
blueWonStr = "Blue Team Won"

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


convertToColor :: String -> CardColor
convertToColor "Red" = Red
convertToColor "Blue" = Blue
convertToColor "Black" = Black
convertToColor "Yellow" = Yellow
convertToColor _ = Yellow


wrapAroundCursor :: Int -> Int -> Int
wrapAroundCursor val n
    | val >= n = val - n
    | val < 0 = val + n
    | otherwise = val

-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

gridSize::Int
gridSize = 5

-- //////////////Player Game State/////////////////

data PlayerGameState = PlayerGameState {
    playerGrid    :: PlayerGrid,
    pWordList      :: [String],
    pCardColor     :: [CardColor],
    pRedTeamScore  :: Int,
    pBlueTeamScore :: Int,
    pTeamTurn      :: CardColor,
    pSpyHint       :: SpyHint,
    pPlayerCursor  :: Coord,
    pSpyMastersTurn :: Bool,
    pSock           :: Socket,
    pWinner        :: CardColor,
    pWait           :: Bool,
    pLogs           :: [String]
} deriving (Show)

-- utility functions
-- updatePlayerCell :: (PlayerCell -> PlayerCell) -> PlayerGameState -> PlayerGameState
-- updatePlayerCell updatePcellFn game = game {playerGrid = playerGrid game & ix y . ix x %~ updatePcellFn }
--   where (Loc x y) = playerCursor game

createPlayerCard :: (String, CardColor, Idx) -> PlayerCell
createPlayerCard (word, color, idx) = PCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) word False color

createPlayerState :: [String] -> [String] -> Socket -> PlayerGameState
createPlayerState wordlis colors sock = createBoard cellList
        where
            tupleList = zip3 wordlis (map convertToColor colors) [0..((gridSize*gridSize) - 1)]
            cellList = map createPlayerCard tupleList
            createBoard :: [PlayerCell] -> PlayerGameState 
            createBoard cls = PlayerGameState 
                                {
                                    pPlayerCursor = (Loc 0 0),
                                    playerGrid = map (getSlice cls) [0..(gridSize - 1)],
                                    pRedTeamScore = 9,
                                    pBlueTeamScore = 8,
                                    pWordList = wordlis,
                                    pCardColor = (map convertToColor colors),
                                    pTeamTurn = Red,
                                    pSpyMastersTurn = False,
                                    pSpyHint = waitingStr,
                                    pSock = sock,
                                    pWinner = Yellow,
                                    pWait = True,
                                    pLogs = []
                                }
            getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis

class GameState a where
    moveCursor :: Direction -> a -> a
    selectCard :: a -> a
    getCardColor :: a -> CardColor
    updateCurrentTurnsAndScore :: a -> a
    updateGame :: a -> a
    updateSpyHint :: a -> SpyHint -> a
    endTurn :: a -> a


instance GameState PlayerGameState where
    moveCursor direction game = game {pPlayerCursor = cur}
                                where
                                    (Loc r c) = pPlayerCursor game
                                    cur = case direction of
                                            UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
                                            DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
                                            LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
                                            RightD -> Loc r (wrapAroundCursor (c+1) gridSize)

    selectCard pb = updatePCard updateFn pb
                        where
                            updateFn :: PlayerCell -> PlayerCell
                            updateFn cell = case cell of
                                                PCell l w True c -> PCell l w True c
                                                PCell l w False c -> PCell l w True c
    getCardColor game = cColor
                            where (Loc x y) = pPlayerCursor game
                                  PCell _ _ _ cColor = playerGrid game !! x !! y
    

    updateCurrentTurnsAndScore game = game {pTeamTurn = updateTeam (teamColor) (cColor) (curHint),
                                            pRedTeamScore = updateRedTeamScore cColor redScore ,
                                            pBlueTeamScore = updateBlueTeamScore cColor blueScore,
                                            pSpyMastersTurn = updateSpyMastersTurn (teamColor) cColor,
                                            pSpyHint = updateCurrentHint (updateTeam (teamColor) (cColor) (curHint)) teamColor curHint,
                                            pWinner = updateWinner (updateRedTeamScore cColor redScore) (updateBlueTeamScore cColor blueScore) (teamColor) (cColor),
                                            pWait = updateWait (updateCurrentHint (updateTeam (teamColor) (cColor) (curHint)) teamColor curHint),
                                            pLogs = updateLogs currLogs teamColor currCard
                                            }
                                            where cColor = getCardColor game
                                                  teamColor = pTeamTurn game
                                                  redScore = pRedTeamScore game
                                                  blueScore = pBlueTeamScore game
                                                  curHint   = pSpyHint game
                                                  currLogs = pLogs game
                                                  currCard = getCard game
    
    updateGame game = if (isPCardClicked currCard)
                        then game
                        else
                            if wait
                                then game
                                else updateCurrentTurnsAndScore (selectCard game)
                        
                        where
                            (Loc x y) = pPlayerCursor game
                            currCard = playerGrid game !! x !! y
                            wait = pWait game
    updateSpyHint game newHint = game {pSpyHint = newHint}

    endTurn game = game {pTeamTurn = switchTeam teamColor,
                         pSpyHint = waitingStr,
                         pWait = True}
                        where
                            teamColor = pTeamTurn game
updateWinner :: (Int) -> (Int) -> (CardColor) -> (CardColor) -> (CardColor)
updateWinner _ _ Red (Black) = Blue
updateWinner _ _ Blue (Black) = Red
updateWinner 0 _ Blue _ = Red
updateWinner _ 0 Blue _ = Blue

updateWinner _ _ _ _ = Yellow
 
switchTeam :: CardColor -> CardColor
switchTeam Blue = Red
switchTeam Red = Blue

getCard :: PlayerGameState -> PlayerCell
getCard game = playerGrid game !! x !! y
                where Loc x y = pPlayerCursor game

updateWait :: String -> Bool
updateWait hint = if hint == waitingStr || hint == redWonStr || hint == blueWonStr
                    then True
                    else False

updateLogs :: [String] -> CardColor -> PlayerCell -> [String]
updateLogs logs teamColor (PCell _ word _ cColor) = if length logs >= 6
                                                            then (tail logs) ++ log
                                                            else logs ++ log
                                                            where
                                                                log = ["Team:" ++ (show teamColor) ++ " selected " ++ word ++ " (" ++ (show cColor) ++ ")"]

instance GameState SpyGameState where
    moveCursor direction game = game {sPlayerCursor = cur}
                                where
                                    (Loc r c) = sPlayerCursor game
                                    cur = case direction of
                                            UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
                                            DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
                                            LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
                                            RightD -> Loc r (wrapAroundCursor (c+1) gridSize)

    selectCard sb = updateSCard updateFn sb
                        where
                            updateFn :: SpyCell -> SpyCell
                            updateFn cell = case cell of
                                                SCell l w True c -> SCell l w True c
                                                SCell l w False c -> SCell l w True c
    getCardColor game = cColor
                            where (Loc x y) = sPlayerCursor game
                                  SCell _ _ _ cColor  = spyGrid game !! x !! y

    updateCurrentTurnsAndScore game = game {sTeamTurn = updateTeam (teamColor) (cColor) (curHint),
                                        sRedTeamScore = updateRedTeamScore cColor redScore ,
                                        sBlueTeamScore = updateBlueTeamScore cColor blueScore,
                                        sSpyMastersTurn = updateSpyMastersTurn teamColor cColor,
                                        sSpyHint = updateCurrentHint (updateTeam (teamColor) (cColor) (curHint)) teamColor curHint,
                                        sWinner = updateWinner (updateRedTeamScore cColor redScore) (updateBlueTeamScore cColor blueScore) (teamColor) (cColor)
                                        }
                                        where cColor = getCardColor game
                                              teamColor = sTeamTurn game
                                              redScore = sRedTeamScore game
                                              blueScore = sBlueTeamScore game
                                              curHint = sSpyHint game
    updateGame game = if (isSCardClicked currCard)
                        then game
                        else updateCurrentTurnsAndScore (selectCard game)
                        
                        where
                            (Loc x y) = sPlayerCursor game
                            currCard = spyGrid game !! x !! y
    updateSpyHint game newHint = game {sSpyHint = newHint}

    endTurn game = game {sTeamTurn = switchTeam teamColor,
                         sSpyHint  = waitingStr}
                        where
                            teamColor = sTeamTurn game

-- moveCursor :: Direction -> PlayerGameState -> PlayerGameState
-- moveCursor direction game = game {playerCursor = cur}
--                                 where
--                                     (Loc r c) = playerCursor game
--                                     cur = case direction of
--                                             UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
--                                             DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
--                                             LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
--                                             RightD -> Loc r (wrapAroundCursor (c+1) gridSize)


updatePCard :: (PlayerCell -> PlayerCell) -> PlayerGameState -> PlayerGameState
updatePCard updateFn pb = pb { playerGrid = playerGrid pb & ix x . ix y %~ updateFn }
                            where (Loc x y) = pPlayerCursor pb


-- selectCard :: PlayerGameState -> PlayerGameState
-- selectCard pb = updateCard updateFn pb
--     where
--         updateFn :: PlayerCell -> PlayerCell
--         updateFn cell = case cell of
--                             PCell l w True c -> PCell l w True c
--                             PCell l w False c -> PCell l w True c


-- getCardColor :: PlayerGameState -> CardColor
-- getCardColor game = cColor
--     where (Loc x y) = playerCursor game
--           PCell _ _ _ cColor = playerGrid game !! x !! y

isPCardClicked :: PlayerCell -> Bool
isPCardClicked (PCell _ _ isClicked _) = isClicked

-- common functions between both game states

updateTeam :: CardColor -> CardColor -> String -> CardColor
updateTeam Red cColor hint       = if (hint == waitingStr) || (getHintCount hint) == 0 || (cColor /= Red) 
                                        then Blue
                                        else Red
updateTeam Blue cColor hint      = if (hint == waitingStr) || (getHintCount hint) == 0 || (cColor /= Blue) 
                                        then Red
                                        else Blue

updateRedTeamScore :: CardColor -> Int -> Int
updateRedTeamScore Red score  = score - 1
updateRedTeamScore _ score = score

updateBlueTeamScore :: CardColor -> Int -> Int
updateBlueTeamScore Blue score  = score - 1
updateBlueTeamScore _ score = score

updateSpyMastersTurn :: CardColor -> CardColor -> Bool
updateSpyMastersTurn Red cColor = if cColor == Red then False else True
updateSpyMastersTurn Blue cColor = if cColor == Blue then False else True

-- updateCurrentTurnsAndScore :: PlayerGameState -> PlayerGameState
-- updateCurrentTurnsAndScore game = game {teamTurn = updateTeam (teamColor) (cColor),
--                                     pRedTeamScore = updateRedTeamScore cColor redScore ,
--                                     pBlueTeamScore = updateBlueTeamScore cColor blueScore,
--                                     spyMastersTurn = updateSpyMastersTurn (teamColor) cColor
--                                     }
--                                 where cColor = getCardColor game
--                                       teamColor = teamTurn game
--                                       redScore = pRedTeamScore game
--                                       blueScore = pBlueTeamScore game
                            

-- updatePlayerGame:: PlayerGameState -> PlayerGameState
-- updatePlayerGame game = if (isCardClicked currCard)
--                         then game
--                         else updateCurrentTurnsAndScore (selectCard game)
                        
--                         where
--                             (Loc x y) = playerCursor game
--                             currCard = playerGrid game !! x !! y

-- functions that should run after new hints from the spy master
-- updateSpyHint :: PlayerGameState -> PlayerGameState
-- updateSpyHint game = game


-- //////////////Spy Game State/////////////////

data SpyGameState = SpyGameState {
    spyGrid        :: SpyGrid,
    sWordList       :: [String],
    sCardColor      :: [CardColor],
    sRedTeamScore   :: Int,
    sBlueTeamScore  :: Int,
    sTeamTurn       :: CardColor,
    sSpyHint        :: SpyHint,
    sSpyMastersTurn :: Bool,
    sPlayerCursor   :: Coord,
    sSock           :: Socket,
    sWinner         :: CardColor
} deriving (Show)


createSpyCard :: (String, CardColor, Idx) -> SpyCell
createSpyCard (word, color, idx) = SCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) word False color

createSpyState :: [String] -> [String] -> Socket -> SpyGameState
createSpyState wordlis colors sock = createBoard cellList
        where
            tupleList = zip3 wordlis (map convertToColor colors) [0..((gridSize*gridSize) - 1)]
            cellList = map createSpyCard tupleList
            createBoard :: [SpyCell] -> SpyGameState 
            createBoard cls = SpyGameState 
                                {
                                    sPlayerCursor = (Loc 0 0),
                                    spyGrid = map (getSlice cls) [0..(gridSize - 1)],
                                    sRedTeamScore = 9,
                                    sBlueTeamScore = 8,
                                    sWordList = wordlis,
                                    sCardColor = (map convertToColor colors),
                                    sTeamTurn = Red,
                                    sSpyMastersTurn = False,
                                    sSpyHint = waitingStr, 
                                    sSock = sock,
                                    sWinner = Yellow
                                }
            getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis

-- updateSpyHint :: PlayerGameState -> PlayerGameState
-- updateSpyHint game = game


-- moveCursor :: Direction -> SpyGameState -> SpyGameState
-- moveCursor direction game = game {playerCursor = cur}
--                                 where
--                                     (Loc r c) = playerCursor game
--                                     cur = case direction of
--                                             UpD -> Loc (wrapAroundCursor (r-1) gridSize) c
--                                             DownD -> Loc (wrapAroundCursor (r+1) gridSize) c
--                                             LeftD -> Loc r (wrapAroundCursor (c-1) gridSize)
--                                             RightD -> Loc r (wrapAroundCursor (c+1) gridSize)

updateSCard :: (SpyCell -> SpyCell) -> SpyGameState -> SpyGameState
updateSCard updateFn sb = sb { spyGrid = spyGrid sb & ix x . ix y %~ updateFn }
  where (Loc x y) = sPlayerCursor sb


-- selectCard :: SpyGameState -> SpyGameState
-- selectCard sb = updateCard updateFn sb
--     where
--         updateFn :: SpyCell -> SpyCell
--         updateFn cell = case cell of
--                             PCell l w True c -> PCell l w True c
--                             PCell l w False c -> PCell l w True c


-- getCardColor :: SpyGameState -> CardColor
-- getCardColor game = cColor
--     where (Loc x y) = playerCursor game
--           PCell _ _ _ cColor = spyGrid game !! x !! y

isSCardClicked :: SpyCell -> Bool
isSCardClicked (SCell _ _ isClicked _) = isClicked



-- updateCurrentTurnsAndScore :: SpyGameState -> SpyGameState
-- updateCurrentTurnsAndScore game = game {teamTurn = updateTeam (teamColor) (cColor),
--                                     sRedTeamScore = updateRedTeamScore cColor redScore ,
--                                     sBlueTeamScore = updateBlueTeamScore cColor blueScore,
--                                     spyMastersTurn = updateSpyMastersTurn (teamColor) cColor
--                                     }
--                                 where cColor = getCardColor game
--                                       teamColor = teamTurn game
--                                       redScore = sRedTeamScore game
--                                       blueScore = sBlueTeamScore game
                            

-- updateSpyGame:: SpyGameState -> SpyGameState
-- updateSpyGame game = if (isCardClicked currCard)
--                         then game
--                         else updateCurrentTurnsAndScore (selectCard game)
                        
--                         where
--                             (Loc x y) = playerCursor game
--                             currCard = spyGrid game !! x !! y

-- functions that should run after new hints from the spy master
-- updateSpyHint :: SpyGameState -> SpyHint -> SpyGameState
-- updateSpyHint game newHint = game {spyHint = newHint}



updateHintFromPlayer :: String -> SpyGameState -> SpyGameState

updateHintFromPlayer msg sb = sb { sSpyHint = (msg) }


getHintCount :: String -> Int
getHintCount curHint = (read (splitOn "," curHint !! 1) :: Int) - 1

updateCurrentHint :: CardColor -> CardColor -> String -> String
updateCurrentHint oldTeamCol newTeamCol curHint =  if curHint == waitingStr
                                                    then waitingStr
                                                    else 
                                                        if oldTeamCol /= newTeamCol
                                                            then waitingStr
                                                            else
                                                                if clueNum > 0
                                                                    then hintSplit!!0 ++ "," ++ show clueNum
                                                                    else waitingStr
                                                                    where
                                                                        hintSplit = splitOn "," curHint
                                                                        clueNum   =  (read (hintSplit!!1) :: Int) - 1

                            

updateHintFromSpy :: String -> PlayerGameState -> PlayerGameState
updateHintFromSpy msg pb = pb { pSpyHint = msg, pWait = False }


invalidHint :: SpyHint -> Bool
invalidHint hint = if not (elem ',' hint)
                        then True
                        else 
                            if not (length hintSplit == 2)
                                then True
                                else
                                    if  (not ((length (hintSplit !! 1)) == 1)) || (not (isDigit ((hintSplit !! 1) !! 0)))
                                        then True
                                        else
                                            if length (hintSplit !! 0) == 0
                                                then True
                                                else False
                            where
                                hintSplit = splitOn "," (strip hint)



updateSelectedCell :: String -> SpyGameState -> SpyGameState
updateSelectedCell msg sb  = if msg == waitingStr
                                    then sb
                                else
                                    (sb { sPlayerCursor = Loc row col})
                                        where
                                            splitMsg = splitOn " " msg
                                            row = read(splitMsg !! 1) :: Int
                                            col = read(splitMsg !! 2) :: Int
data Hint = WordCountField
          deriving (Eq, Ord, Show)

data SpyStateAndForm = SpyStateAndForm { _wordCount      :: E.Editor T.Text Hint,
                                _spyState :: SpyGameState} 
    deriving (Show)

makeLenses ''SpyStateAndForm        

                   





