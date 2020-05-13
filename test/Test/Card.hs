module Test.Card where

import Card
import Test.QuickCheck

instance Arbitrary Card where
  arbitrary = do
    v <- elements [Two .. Ace]
    s <- elements [Clubs .. Spades]
    return (Card v s)
