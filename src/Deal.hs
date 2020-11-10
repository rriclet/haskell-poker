module Deal
( Deal
, dealHoleCards
) where

import Card
import Common
import Deck
import Control.Monad
import Control.Monad.Random.Class
import Data.Maybe

data Deal = Deal {
  deck :: Deck,
  players :: [(Player, Hole)],
  moves :: [Move],
  community :: [Card],
  smallBlind :: (Player, Chips),
  bigBlind :: (Player, Chips),
  pot :: Chips,
  winners :: [Player]
} deriving (Eq)

{-
play :: StartDeal -> Deal
play sd = dealAllHole (deck sd) (players sd)
-}

dealHoleCards :: Deck -> [Player] -> ([(Player, Hole)], Deck)
dealHoleCards deck players = (playersWithCards, rest)
  where 
    playersWithCards = zip players holeCards
    holeCards        = zip firstN nextN
    firstN           = take n deck
    nextN            = take n . drop n $ deck
    rest             = drop (n * 2) deck
    n                = length players
