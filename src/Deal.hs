module Deal
( Deal
, Dealing
, dealAllHole
) where

import Card
import Common
import Deck

data Deal = Deal {
  players :: [(Player, Hole)],
  moves :: [Move],
  community :: [Card],
  smallBlind :: (Player, Chips),
  bigBlind :: (Player, Chips),
  pot :: Chips,
  winners :: [Player]
} deriving (Eq)

type Dealing = ([(Player, Hole)], Deck)

dealAllHole :: Deck -> [Player] -> Maybe Dealing
dealAllHole d = foldl dealAllHole' (Just ([], d))

dealAllHole' :: Maybe Dealing -> Player -> Maybe Dealing
dealAllHole' Nothing p       = Nothing
dealAllHole' (Just (h, d)) p = case dealHole d p of
  Just (h', d') -> Just (h' : h, d')
  Nothing       -> Nothing

dealHole :: Deck -> Player -> Maybe ((Player, Hole), Deck)
dealHole (x1:x2:xs) p = Just ((p, (x1, x2)), xs)
dealHole _ p          = Nothing
