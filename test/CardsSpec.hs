module CardsSpec (spec) where

import Test.Hspec
import Cards
import Data.List

spec :: Spec
spec = do

  describe "Cards.deck" $ do
    it "returns a deck of 52 cards" $ do
      length deck `shouldBe` 52
    it "returns a deck of unique cards" $ do
      nub deck `shouldBe` deck
