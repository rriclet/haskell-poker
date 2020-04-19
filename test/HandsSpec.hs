module HandsSpec (spec) where

import Test.Hspec
import Hands
import Cards

spec :: Spec
spec = do

  describe "Hands.bestHand" $ do
    it "returns the best" $ do
      bestHand [Card Ace Spades] `shouldBe` (HighCard, [Card Ace Spades])

-- Write tests for :
-- bestHand royalFlush straightFlush fourOfKind fullHouse
-- flush straight threeOfKind twoPair onePair highCard
