module Test.Common where

import Common
import Test.QuickCheck

instance Arbitrary Player where
  arbitrary = Player <$> arbitrary <*> arbitrary