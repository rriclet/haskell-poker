{-# LANGUAGE TemplateHaskell #-}
module Test.Deal where

import Card
import Common
import Deal
import Deck
import Test.Card
import Test.Common
import Test.QuickCheck

-- If dealAllHole succeded, 
-- there should be at least 2 cards per player in the Deck 
prop_dealAllHoleSize :: Deck -> [Player] -> Bool
prop_dealAllHoleSize d p = case dealAllHole d p of
  Just _  -> length d >= length p * 2
  _       -> True

-- Check that the players were given the cards on top of the Deck
prop_dealAllHoleOrder :: Deck -> [Player] -> Bool
prop_dealAllHoleOrder d p = case dealAllHole d p of
  Just (xs, _) -> holes xs == take (length $ holes xs) d
  _ -> True
  where holes = foldr ((\x y -> fst x : snd x : y) . snd) [] . reverse

return []
runTests :: IO Bool
runTests = $quickCheckAll
