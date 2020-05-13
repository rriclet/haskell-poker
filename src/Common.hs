module Common where

import Card

data Player = Player {
  name :: String,
  chips :: Chips
} deriving (Eq, Show)

type Chips = Int

type Hole = (Card, Card)

type Move = (Player, Action)

data Action = Bet Chips
            | Call Chips
            | Check
            | Fold
            | Raise Chips
            | AllIn Chips
  deriving (Eq)