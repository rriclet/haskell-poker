module Deck 
( Deck
, deck
) where

import Card
import System.Random.Shuffle
import Control.Monad.Random.Class

type Deck = [Card]

deck :: MonadRandom m => m Deck
deck = shuffleM (Card <$> [Two .. Ace] <*> [Clubs .. Spades])
