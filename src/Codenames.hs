module Codenames (
    initialGame,
    getMainMenu,
    Codenames(..),
    Name(..),
    MenuList
) where

import Game

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

data Name = ResName
    deriving (Eq, Ord, Show)
type MenuList = L.List Name String

-- | The current status of the game
data Codenames
  =   MainMenu MenuList
    | PlayerView PlayerGameState


getMainMenu :: Codenames
getMainMenu = MainMenu menuOptions

-- | Creates an initial 'Game' starting at the main menu
initialGame :: Codenames
initialGame = PlayerView pb


downloadedColorList :: [CardColor]
downloadedColorList = [Red, Red, Red, Red, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Black, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]
egwordList :: [String]
egwordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

pb :: PlayerGameState
pb = createPlayerGrid egwordList downloadedColorList


menuOptions :: L.GenericList Name Vec.Vector String
menuOptions = L.list ResName (Vec.fromList ["Player", "Spymaster"]) 2
