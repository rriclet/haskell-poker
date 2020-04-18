module Cards where

data Value = Two | Three | Four | Five | Six 
           | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
  deriving (Show, Enum, Eq, Ord)

data Suit = Clubs | Diamonds | Hearts | Spades 
  deriving (Show, Enum, Eq, Ord)

data Card = Card { value :: Value, suit :: Suit }
  deriving (Eq, Ord)

instance Show Card where show (Card value suit) = "Card " ++ show value ++ " " ++ show suit

type Deck = [Card]

deck :: Deck
deck = [Card value suit | value <- [Two .. Ace], suit <- [Clubs .. Spades] ]
