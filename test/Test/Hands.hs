{-# LANGUAGE TemplateHaskell #-}
module Test.Hands where

import Cards
import Hands
import Test.QuickCheck
import Data.List
import Data.Ord

instance Arbitrary Card where
  arbitrary = do
    v <- elements [Two .. Ace]
    s <- elements [Clubs .. Spades]
    return (Card v s)

-- For OnePair / TwoPair / ThreeOfKind : missing test to check for highest value possible

sameSuit :: [Card] -> Bool
sameSuit [] = False
sameSuit l@(x:_) = all (\c -> suit x == suit c) l

sameValue :: [Card] -> Bool
sameValue [] = False
sameValue l@(x:_) = all (\c -> value x == value c) l

isSublist :: [Card] -> [Card] -> Bool
isSublist s l = s `intersect` l == s

isOrdered :: [Card] -> Bool
isOrdered l = sortOn (Down . value) l == l

valuesFollowing :: [Card] -> Bool
valuesFollowing []         = False
valuesFollowing [_]        = True
valuesFollowing (x1:x2:xs) = after (value x1) == value x2 && valuesFollowing (x2 : xs)

prop_bestHand :: [Card] -> Bool
prop_bestHand xs
  | null xs   = bh == (None, [])
  | otherwise = True
 where bh = bestHand xs

-- For all, we must check for correct length, order and if result is a sublist of the given cards 

prop_royalFlush :: [Card] -> Bool
prop_royalFlush xs
  | null rf   = True
  | otherwise = length rf == 5 && isRoyal rf && sameSuit rf 
 where rf = royalFlush xs

prop_straightFlush :: [Card] -> Bool
prop_straightFlush xs
  | null sf   = True
  | otherwise = length sf == 5 && sf `isSublist` xs && valuesFollowing sf && sameSuit sf
 where sf = straightFlush xs

prop_fourOfKind :: [Card] -> Bool
prop_fourOfKind xs 
  | null fk   = True
  | otherwise = length fk >= 4 && fk `isSublist` xs && sameValue (take 4 fk)
 where fk = fourOfKind xs

prop_fullHouse :: [Card] -> Bool
prop_fullHouse xs
  | null fh   = True
  | otherwise = length fh == 5 && fh `isSublist` xs && sameValue tk && sameValue op
 where fh = fullHouse xs
       tk = take 3 fh
       op = take 2 $ drop 3 fh  

prop_flush :: [Card] -> Bool
prop_flush xs
  | null fl   = True
  | otherwise = length fl == 5 && fl `isSublist` xs && sameSuit fl 
 where fl = flush xs

prop_straight :: [Card] -> Bool
prop_straight xs 
  | null st   = True
  | otherwise = length st == 5 && st `isSublist` xs && valuesFollowing st
 where st = straight xs

prop_threeOfKind :: [Card] -> Bool
prop_threeOfKind xs 
  | null tk   = True 
  | otherwise = tk `isSublist` xs && length tk >= 3 && sameValue (take 3 tk)
 where tk = threeOfKind xs

prop_twoPair :: [Card] -> Bool
prop_twoPair xs
  | null tp   = True
  | otherwise = tp `isSublist` xs && length tp >= 4 && sameValue pair1 && sameValue pair2 
 where tp = twoPair xs
       pair1 = take 2 tp
       pair2 = take 2 pair1

prop_onePair :: [Card] -> Bool
prop_onePair xs
  | null op   = True
  | otherwise = op `isSublist` xs && length op >= 2 && sameValue (take 2 op) 
 where op = onePair xs 

prop_highCard :: Int -> [Card] -> Bool
prop_highCard n xs 
  | n <= 0 && null hc = True
  | length xs < n     = common && length hc == length xs
  | otherwise         = common && length hc == n
 where hc     = highCard n xs
       common = hc `isSublist` xs && isOrdered hc

return []
runTests :: IO Bool
runTests = $quickCheckAll
