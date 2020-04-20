module Hands 
( Hand(..)
, bestHand
, royalFlush
, straightFlush
, fourOfKind
, fullHouse
, flush
, straight
, threeOfKind
, twoPair
, onePair
, highCard
) where

import Cards
import Data.List 
import Data.Ord

data Hand = None | HighCard | OnePair | TwoPair | ThreeOfKind 
           | Straight | Flush | FullHouse | FourOfKind 
           | StraightFlush | RoyalFlush 
  deriving (Show, Eq, Ord)

-- If multiple equal hands, gives them in Suit Enum order
bestHand :: [Card] -> (Hand, [Card])
bestHand xs
  | null xs       = (None, [])
  | not (null rf) = (RoyalFlush, rf)
  | not (null sf) = (StraightFlush, sf)
  | not (null fk) = (FourOfKind, fk)
  | not (null fh) = (FullHouse, fh)
  | not (null fl) = (Flush, fl)
  | not (null st) = (Straight, st)
  | not (null tk) = (ThreeOfKind, tk)
  | not (null tp) = (TwoPair, tp)
  | not (null op) = (OnePair, op)
  | otherwise     = (HighCard, hc)
    where rf = royalFlush xs
          sf = straightFlush xs
          fk = fourOfKind xs
          fh = fullHouse xs
          fl = flush xs
          st = straight xs
          tk = threeOfKind xs
          tp = twoPair xs
          op = onePair xs
          hc = highCard 5 xs

royalFlush :: [Card] -> [Card]
royalFlush xs = if sumStraight == sumRoyal then straightFlush xs else []
                  where sumRoyal    = sum $ map fromEnum [Ace,King,Queen,Jack,Ten]
                        sumStraight = sum $ map (fromEnum . value) $ straightFlush xs 

straightFlush :: [Card] -> [Card]
straightFlush = concat . take 1 . sortOn (Down . value . head) . map straight . filter (\x -> length x >= 5) . bySuit . sortOn value

fourOfKind :: [Card] -> [Card]
fourOfKind xs = if not (null fk) then fk ++ hc else []
                  where fk = nSameCards 4 xs
                        hc = highCard 1 $ xs \\ fk

fullHouse :: [Card] -> [Card]
fullHouse xs = if not (null tk || null op) then tk ++ op else []
                where tk = take 3 $ threeOfKind xs
                      op = take 2 $ onePair $ xs \\ tk

flush :: [Card] -> [Card]
flush = enoughOrEmpty 5 . concat . sortOn (Down . length) . filter (\x -> length x >= 5) . bySuit . sortOn (Down . value)

straight :: [Card] -> [Card]
straight = enoughOrEmpty 5 . foldl straightFold [] . copyAces . nubBy sameValue . sortOn (Down . value)
straightFold l c
          | null l        = [c]
          | length l == 5 = l
          | canAdd        = c : l
          | otherwise     = [c]           
          where canAdd = after (value c) == value (head l) 

-- we copy Ace's to the end to get Straight's like A2345
copyAces :: [Card] -> [Card]
copyAces xs = xs ++ filter (\x -> value x == Ace) xs

threeOfKind :: [Card] -> [Card]
threeOfKind xs = if not (null tk) then tk ++ hc else []
                  where tk = nSameCards 3 xs
                        hc = highCard 2 $ xs \\ tk

twoPair :: [Card] -> [Card]
twoPair xs = if not (null tp1 || null tp2) then tp1 ++ tp2 ++ hc else []
                where tp1 = take 2 $ onePair xs
                      tp2 = take 2 $ onePair (xs \\ tp1)                      
                      hc = highCard 1 $ xs \\ (tp1 ++ tp2)

onePair :: [Card] -> [Card]
onePair xs = if not (null op) then op ++ hc else []
                where op = nSameCards 2 xs
                      hc = highCard 3 $ xs \\ op

highCard :: Int -> [Card] -> [Card]
highCard n = take n . sortOn (Down . value) 

bySuit :: [Card] -> [[Card]]
bySuit xs = [ [ x | x <- xs, suit x == s ] | s <- [Clubs .. Spades] ]

byValue :: [Card] -> [[Card]]
byValue xs = reverse [ [ x | x <- xs, value x == v ] | v <- [Two .. Ace] ]

nSameCards :: Int -> [Card] -> [Card]
nSameCards n = enoughOrEmpty n . concat . take 1 . sortOn (Down . value . head) . filter (\x -> length x >= n) . groupBy sameValue . sortOn value

sameValue :: Card -> Card -> Bool
sameValue x y = value x == value y

enoughOrEmpty :: Int -> [a] -> [a]
enoughOrEmpty n xs = if length xs >= n then take n xs else []
