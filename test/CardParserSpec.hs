module CardParserSpec (spec) where

import Test.Hspec
import Cards
import CardParser

spec :: Spec
spec = do

  describe "CardParser.p" $ do
    it "returns all parsed cards" $ do
      p ["2c", "Ks", "2h", "2s", "6s", "9d", "2d"] `shouldBe` [Card Two Clubs, Card King Spades, Card Two Hearts, Card Two Spades, Card Six Spades, Card Nine Diamonds, Card Two Diamonds]
    it "removes incorrect cards" $ do
      p ["2c", "Ps", "6y", "9d", "2d"] `shouldBe` [Card Two Clubs, Card Nine Diamonds, Card Two Diamonds]
