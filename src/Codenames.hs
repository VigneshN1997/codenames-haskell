module Codenames (
    Codenames(..),
    ConnectionTick(..),
    MessageS(..),
    Name(..)
) where

import Game


data Name = ResName
    deriving (Eq, Ord, Show)

-- | The current status of the game
data Codenames
  =  PlayerView PlayerGameState
    | SpyView SpyStateAndForm

data MessageS = S_Str String

newtype ConnectionTick = ConnectionTick MessageS