module Cards 
( Value(..)
, before
, after
, Suit(..)
, Card(..)
, Deck
, deck
) where

data Value = Two | Three | Four | Five | Six 
           | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
  deriving (Show, Enum, Eq, Ord)

class (Enum a) => Adjacent a where 
  before, after :: a -> a

-- used to determine valid Straight's
instance Adjacent Value where
  before Two = Ace
  before x   = pred x
  after Ace  = Two
  after x    = succ x

data Suit = Clubs | Diamonds | Hearts | Spades 
  deriving (Show, Enum, Eq, Ord)

data Card = Card { value :: Value, suit :: Suit }
  deriving (Eq, Ord)

instance Show Card where show (Card value suit) = "Card " ++ show value ++ " " ++ show suit

type Deck = [Card]

deck :: Deck
deck = [Card value suit | value <- [Two .. Ace], suit <- [Clubs .. Spades] ]
