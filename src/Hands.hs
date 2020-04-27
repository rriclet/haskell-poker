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
, isRoyal
) where

import Cards
import Data.List 
import Data.Ord

data Hand = None | HighCard | OnePair | TwoPair | ThreeOfKind 
           | Straight | Flush | FullHouse | FourOfKind 
           | StraightFlush | RoyalFlush 
  deriving (Show, Enum, Eq, Ord)

-- If multiple equal hands, gives them in Suit Enum order
bestHand :: [Card] -> (Hand, [Card])
bestHand = bestHand' (reverse [HighCard .. RoyalFlush])

bestHand' :: [Hand] -> [Card] -> (Hand, [Card])
bestHand' (h:hs) c = if null best then bestHand' hs c else (h, best)
                      where best = fHand h c
bestHand' _ _ = (None, [])

fHand :: Hand -> ([Card] -> [Card])
fHand h = case h of 
               RoyalFlush -> royalFlush
               StraightFlush -> straightFlush
               FourOfKind -> fourOfKind
               FullHouse -> fullHouse
               Flush -> flush
               Straight -> straight
               ThreeOfKind -> threeOfKind
               TwoPair -> twoPair
               OnePair -> onePair
               _ -> highCard 5

royalFlush :: [Card] -> [Card]
royalFlush xs = let sf = straightFlush xs
                in if isRoyal sf then sf else []  

straightFlush :: [Card] -> [Card]
straightFlush = concat . take 1 . sortOn (Down . value . head) . filter (not . null) . map straight . filter (\x -> length x >= 5) . bySuit . sortOn value

fourOfKind :: [Card] -> [Card]
fourOfKind xs = if not (null fk) then fk ++ hc else []
                  where fk = nSameCards 4 xs
                        hc = highCard 1 $ xs \\ fk

fullHouse :: [Card] -> [Card]
fullHouse xs = if not (null tk || null op) then tk ++ op else []
                where tk = take 3 $ threeOfKind xs
                      op = take 2 $ onePair $ xs \\ tk

flush :: [Card] -> [Card]
flush = enoughOrEmpty 5 . concat . sortOn (Down . value . head) . filter (\x -> length x >= 5) . bySuit . sortOn (Down . value)

straight :: [Card] -> [Card]
straight = enoughOrEmpty 5 . foldl straightFold [] . copyAces . nubBy sameValue . sortOn (Down . value)

straightFold :: [Card] -> Card -> [Card]
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

isRoyal :: [Card] -> Bool
isRoyal xs = sort (map value xs) == [Ten, Jack, Queen, King, Ace]

bySuit :: [Card] -> [[Card]]
bySuit xs = [ [ x | x <- xs, suit x == s ] | s <- [Clubs .. Spades] ]

nSameCards :: Int -> [Card] -> [Card]
nSameCards n = enoughOrEmpty n . concat . take 1 . sortOn (Down . value . head) . filter (\x -> length x >= n) . groupBy sameValue . sortOn value

sameValue :: Card -> Card -> Bool
sameValue x y = value x == value y

enoughOrEmpty :: Int -> [a] -> [a]
enoughOrEmpty n xs = if length xs >= n then take n xs else []
