module Codenames (
    getMainMenu,
    Codenames(..),
    ConnectionTick(..),
    MessageS(..),
    Name(..),
    MenuList
) where

import Game

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

data Name = ResName
    deriving (Eq, Ord, Show)
type MenuList = L.List Hint String

-- | The current status of the game
data Codenames
  =   MainMenu MenuList
    | PlayerView PlayerGameState
    | SpyView SpyStateAndForm

data MessageS = S_Str String

newtype ConnectionTick = ConnectionTick MessageS

getMainMenu :: Codenames
getMainMenu = MainMenu menuOptions

-- | Creates an initial 'Game' starting at the main menu
-- initialGamePlayer :: Codenames
-- initialGamePlayer = PlayerView pb

-- initialGameSpy :: Codenames
-- initialGameSpy = SpyView sb

-- downloadedColorList :: [CardColor]
-- downloadedColorList = [Red, Red, Red, Red, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Black, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]
-- egwordList :: [String]
-- egwordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

-- pb :: PlayerGameState
-- pb = createPlayerState egwordList downloadedColorList

-- sb :: SpyGameState
-- sb = createSpyState egwordList downloadedColorList


menuOptions :: L.GenericList Hint Vec.Vector String
menuOptions = L.list WordCountField (Vec.fromList ["Player", "Spymaster"]) 2
