module Codenames (
    initialGame,
    Codenames(..)
) where

import Game


-- | The current status of the game
data Codenames
  = PlayerView PlayerBoard


-- | Creates an initial 'Game' starting at the main menu
initialGame :: Codenames
initialGame = PlayerView pb


downloadedColorList :: [CardColor]
downloadedColorList = [Red, Red, Red, Red, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Blue, Black, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow, Yellow]
wordList :: [String]
wordList = ["COLD", "DEATH", "DIAMOND", "DOG", "DRESS", "FRANCE", "FIRE", "GLOVE", "GOLD", "HAND", "JACK", "LONDON", "NEW YORK", "SNOW", "WATCH", "ALASKA", "FROG", "FROST", "CHAIN", "CHRISTMAS", "COMB", "JEWELER", "HAIR", "LOVE", "STORY"]

pb :: PlayerBoard
pb = createPlayerGrid wordList downloadedColorList
