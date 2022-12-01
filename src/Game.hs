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
  , SpyStateAndForm(..)
  , Hint(..)
) where

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
    pWordList      :: [String],
    pCardColor     :: [CardColor],
    pRedTeamScore  :: Int,
    pBlueTeamScore :: Int,
    pTeamTurn      :: CardColor,
    pSpyHint       :: SpyHint,
    pPlayerCursor  :: Coord,
    pSpyMastersTurn :: Bool
} deriving (Show)

-- utility functions
-- updatePlayerCell :: (PlayerCell -> PlayerCell) -> PlayerGameState -> PlayerGameState
-- updatePlayerCell updatePcellFn game = game {playerGrid = playerGrid game & ix y . ix x %~ updatePcellFn }
--   where (Loc x y) = playerCursor game

createPlayerCard :: (String, CardColor, Idx) -> PlayerCell
createPlayerCard (word, color, idx) = PCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) word False color

createPlayerState :: [String] -> [CardColor] -> PlayerGameState
createPlayerState wordlis colors = createBoard cellList
        where
            tupleList = zip3 wordlis colors [0..((gridSize*gridSize) - 1)]
            cellList = map createPlayerCard tupleList
            createBoard :: [PlayerCell] -> PlayerGameState 
            createBoard cls = PlayerGameState 
                                {
                                    pPlayerCursor = (Loc 0 0),
                                    playerGrid = map (getSlice cls) [0..(gridSize - 1)],
                                    pRedTeamScore = 0,
                                    pBlueTeamScore = 0,
                                    pWordList = egwordList,
                                    pCardColor = downloadedColorList,
                                    pTeamTurn = Red,
                                    pSpyMastersTurn = False,
                                    pSpyHint = SHint "Temperature" 2
                                }
            getSlice lis n = slice (n*gridSize) (n*gridSize + (gridSize - 1)) lis

class GameState a where
    moveCursor :: Direction -> a -> a
    selectCard :: a -> a
    getCardColor :: a -> CardColor
    updateCurrentTurnsAndScore :: a -> a
    updateGame :: a -> a
    updateSpyHint :: a -> SpyHint -> a


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

    updateCurrentTurnsAndScore game = game {pTeamTurn = updateTeam (teamColor) (cColor),
                                            pRedTeamScore = updateRedTeamScore cColor redScore ,
                                            pBlueTeamScore = updateBlueTeamScore cColor blueScore,
                                            pSpyMastersTurn = updateSpyMastersTurn (teamColor) cColor
                                            }
                                            where cColor = getCardColor game
                                                  teamColor = pTeamTurn game
                                                  redScore = pRedTeamScore game
                                                  blueScore = pBlueTeamScore game
    updateGame game = if (isPCardClicked currCard)
                        then game
                        else updateCurrentTurnsAndScore (selectCard game)
                        
                        where
                            (Loc x y) = pPlayerCursor game
                            currCard = playerGrid game !! x !! y
    updateSpyHint game newHint = game {pSpyHint = newHint}

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
                                  SCell _ _ _ cColor = spyGrid game !! x !! y

    updateCurrentTurnsAndScore game = game {sTeamTurn = updateTeam (teamColor) (cColor),
                                        sRedTeamScore = updateRedTeamScore cColor redScore ,
                                        sBlueTeamScore = updateBlueTeamScore cColor blueScore,
                                        sSpyMastersTurn = updateSpyMastersTurn (teamColor) cColor
                                        }
                                        where cColor = getCardColor game
                                              teamColor = sTeamTurn game
                                              redScore = sRedTeamScore game
                                              blueScore = sBlueTeamScore game
    updateGame game = if (isSCardClicked currCard)
                        then game
                        else updateCurrentTurnsAndScore (selectCard game)
                        
                        where
                            (Loc x y) = sPlayerCursor game
                            currCard = spyGrid game !! x !! y
    updateSpyHint game newHint = game {sSpyHint = newHint}

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

data Hint = WordField
          | CountField
          deriving (Eq, Ord, Show)

data SpyStateAndForm = SpyStateAndForm { _word      :: String
                            , _count      :: WCount
                            , _spyState        :: SpyGameState
                            } deriving (Show)

makeLenses ''SpyStateAndForm

data SpyGameState = SpyGameState {
    spyGrid        :: SpyGrid,
    sWordList       :: [String],
    sCardColor      :: [CardColor],
    sRedTeamScore   :: Int,
    sBlueTeamScore  :: Int,
    sTeamTurn       :: CardColor,
    sSpyHint        :: SpyHint,
    sSpyMastersTurn :: Bool,
    sPlayerCursor   :: Coord
} deriving (Show)

-- utility functions

-- main functions




createSpyCard :: (String, CardColor, Idx) -> SpyCell
createSpyCard (word, color, idx) = SCell (Loc (idx `div` gridSize) (idx `mod` gridSize)) word False color

createSpyState :: [String] -> [CardColor] -> SpyGameState
createSpyState wordlis colors = createBoard cellList
        where
            tupleList = zip3 wordlis colors [0..((gridSize*gridSize) - 1)]
            cellList = map createSpyCard tupleList
            createBoard :: [SpyCell] -> SpyGameState 
            createBoard cls = SpyGameState 
                                {
                                    sPlayerCursor = (Loc 0 0),
                                    spyGrid = map (getSlice cls) [0..(gridSize - 1)],
                                    sRedTeamScore = 0,
                                    sBlueTeamScore = 0,
                                    sWordList = egwordList,
                                    sCardColor = downloadedColorList,
                                    sTeamTurn = Red,
                                    sSpyMastersTurn = False,
                                    sSpyHint = SHint "Temperature" 2
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









        


                   





