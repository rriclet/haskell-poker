module HandsSpec (spec) where

import Test.Hspec
import Hands
import Cards
import CardParser

spec :: Spec
spec = do

  describe "Hands.bestHand" $ do
    it "returns the best" $ do
      bestHand [Card Ace Spades] `shouldBe` (HighCard, [Card Ace Spades])

-- Write tests for :
-- bestHand

  describe "Hands.royalFlush" $ do
    it "returns a classic Royal Flush" $ do
      royalFlush (p ["Ts", "7d", "Qs", "Js", "6h", "As", "Ks"]) `shouldBe` p ["Ts", "Js", "Qs", "Ks", "As"]

  describe "Hands.straightFlush" $ do
    it "returns a classic Straight Flush" $ do
      straightFlush (p ["Ts", "7d", "9s", "8s", "6h", "7s", "6s"]) `shouldBe` p ["6s", "7s", "8s", "9s", "Ts"]
    it "returns a five high Straight Flush" $ do
      straightFlush (p ["Ts", "As", "3s", "2s", "5h", "4s", "5s"]) `shouldBe` p ["As", "2s", "3s", "4s", "5s"]

  describe "Hands.fourOfKind" $ do
    it "returns a four of a kind and high card" $ do
      fourOfKind (p ["2c", "7s", "2h", "2s", "6s", "9d", "2d"]) `shouldBe` p ["2c", "2h", "2s", "2d", "9d"]

  describe "Hands.fullHouse" $ do
    it "returns a classic Full House" $ do
      fullHouse (p ["8h", "Jd", "3h", "8s", "8d", "Jh", "Qh"]) `shouldBe` p ["8h", "8s", "8d", "Jd", "Jh"]
    it "returns highest Full House possible" $ do
      fullHouse (p ["8h", "Jd", "3h", "8s", "8d", "Jh", "Js"]) `shouldBe` p ["Jd", "Jh", "Js", "8h", "8s"]

  describe "Hands.flush" $ do
    it "returns a classic Flush" $ do
      flush (p ["4h", "Jd", "3h", "As", "8h", "Kh", "Qh"]) `shouldBe` p ["Kh", "Qh", "8h", "4h", "3h"]
    it "returns highest Flush possible" $ do
      flush (p ["4h", "9h", "3h", "6hs", "8h", "Kh", "Qh"]) `shouldBe` p ["Kh", "Qh", "9h", "8h", "6h"]

  describe "Hands.straight" $ do
    it "returns an Ace high straight" $ do
      straight (p ["Tc", "Jd", "Ad", "As", "8h", "Kd", "Qh"]) `shouldBe` p ["Tc", "Jd", "Qh", "Kd", "Ad"]
    it "returns a five high straight" $ do
      straight (p ["2c", "4d", "Ad", "5s", "8h", "Kd", "3h"]) `shouldBe` p ["Ad", "2c", "3h", "4d", "5s"]

  describe "Hands.threeOfKind" $ do
    it "returns a three of a kind (7 cards)" $ do
      threeOfKind (p ["2c", "7s", "9h", "9s", "6s", "9d", "2d"]) `shouldBe` p ["9h", "9s", "9d", "7s", "6s"]
    it "returns a three of a kind (4 cards)" $ do
      threeOfKind (p ["9h", "Jc", "9s", "9d"]) `shouldBe` p ["9h", "9s", "9d", "Jc"]
    it "returns a three of a kind (3 cards)" $ do
      threeOfKind (p ["9h", "9s", "9d"]) `shouldBe` p ["9h", "9s", "9d"]

  describe "Hands.twoPair" $ do
    it "returns two pair (7 cards)" $ do
      twoPair (p ["5h", "8s", "Jh", "8d", "Qh", "4c", "Jc"]) `shouldBe` p ["Jh", "Jc", "8s", "8d", "Qh"]
    it "returns two highest pairs (7 cards)" $ do
      twoPair (p ["4h", "8s", "Jh", "8d", "2s", "4c", "Jc"]) `shouldBe` p ["Jh", "Jc", "8s", "8d", "4h"]
    it "returns two pairs (4 cards)" $ do
      twoPair (p ["4h", "Jh", "4c", "Jc"]) `shouldBe` p ["Jh", "Jc", "4h", "4c"]

  describe "Hands.onePair" $ do
    it "returns a pair (7 cards)" $ do
      onePair (p ["5h", "8s", "Jh", "8d", "Qh", "4c", "3c"]) `shouldBe` p ["8s", "8d", "Qh", "Jh", "5h"]
    it "returns a pair (3 cards)" $ do
      onePair (p ["8s", "Jh", "8d"]) `shouldBe` p ["8s", "8d", "Jh"]
    it "returns a pair (2 cards)" $ do
      onePair (p ["8s", "8d"]) `shouldBe` p ["8s", "8d"]

  describe "Hands.highCard" $ do
    it "returns highest 5 cards" $ do
      highCard 5 (p ["5h", "8s", "Jh", "2d", "Qh", "4c", "3c"]) `shouldBe` p ["Qh", "Jh", "8s", "5h", "4c"]
    it "returns all cards if less than requested cards" $ do
      highCard 5 (p ["5h", "Qh", "4c", "3c"]) `shouldBe` p ["Qh", "5h", "4c", "3c"]
    it "returns correct number of requested cards" $ do
      highCard 2 (p ["5h", "Qh", "4c", "3c"]) `shouldBe` p ["Qh", "5h"]

