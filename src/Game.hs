module Game 
( Game
) where

import Common
import Deal

data Game = Game {
  players :: [Player],
  winners :: [Player],
  deals :: [Deal]
} deriving (Eq)

