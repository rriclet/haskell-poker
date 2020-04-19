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
  | null xs        = (None, [])
  | length rf == 5 = (RoyalFlush, rf)
  | length sf == 5 = (StraightFlush, sf)
  | length fk == 5 = (FourOfKind, fk)
  | length fh == 5 = (FullHouse, fh)
  | length fl == 5 = (Flush, fl)
  | length st == 5 = (Straight, st)
  | length tk == 5 = (ThreeOfKind, tk)
  | length tp == 5 = (TwoPair, tp)
  | length op == 5 = (OnePair, op)
  | otherwise      = (HighCard, hc)
    where rf  = royalFlush xs
          sf  = straightFlush xs
          fk  = fourOfKind xs
          fh  = fullHouse xs
          fl  = flush xs
          st  = straight xs
          tk  = threeOfKind xs
          tp  = twoPair xs
          op  = onePair xs
          hc  = highCard 5 xs

royalFlush :: [Card] -> [Card]
royalFlush = take 5 . concat . filter (\x -> length x == 5) . map (filter $ valueOrHigher Ten) . bySuit

straightFlush :: [Card] -> [Card]
straightFlush = straight' True 

fourOfKind :: [Card] -> [Card]
fourOfKind xs = fst fkAndLeft ++ highCard 1 (snd fkAndLeft)
              where fkAndLeft = handAndCardsLeft xs 4

fullHouse :: [Card] -> [Card]
fullHouse xs = fst (threeOfKind' xs) ++ fst (onePair' (snd $ threeOfKind' xs))

flush :: [Card] -> [Card]
flush = take 5 . concat . sortOn (Down . length) . filter (\x -> length x >= 5) . bySuit . sortOn (Down . value)

straight :: [Card] -> [Card]
straight = straight' False

straight' :: Bool -> [Card] -> [Card]
straight' f = foldl straightFold [] . copyAces .  sortOn (Down . value)
  where straightFold l c
          | null l              = [c]
          | isSameOrEnded       = l
          | canAdd              = c : l
          | otherwise           = [c]
          where lastValue     = value (head l)
                isSameOrEnded = value c == lastValue || length l == 5
                canAdd        = if f then flush else notFlush
                flush         = suit c == suit (head l) && notFlush
                notFlush      = isSucc || canAddAce
                isAce         = value c == Ace  
                isSucc        = not isAce && succ (value c) == lastValue 
                canAddAce     = isAce && length l == 4 && lastValue == Two

-- we copy Ace's to the end to get Straight's like A2345
copyAces :: [Card] -> [Card]
copyAces xs = xs ++ filter (\x -> value x == Ace) xs

threeOfKind :: [Card] -> [Card]
threeOfKind xs = fst (threeOfKind' xs) ++ highCard 2 (snd $ threeOfKind' xs)
threeOfKind' :: [Card] -> ([Card], [Card])
threeOfKind' xs = handAndCardsLeft xs 3

twoPair :: [Card] -> [Card]
twoPair xs = fst (onePair' xs) ++ fst secondPair ++ highCard 1 (snd secondPair)
              where secondPair = onePair' (snd $ onePair' xs)

onePair :: [Card] -> [Card]
onePair xs = fst (onePair' xs) ++ highCard 3 (snd $ onePair' xs)
onePair' :: [Card] -> ([Card], [Card])
onePair' xs = handAndCardsLeft xs 2

highCard :: Int -> [Card] -> [Card]
highCard n = take n . sortOn (Down . value) 

handAndCardsLeft :: [Card] -> Int -> ([Card], [Card])
handAndCardsLeft xs n = (hand, xs \\ hand) 
                          where hand = take n $ concat $ filter (\x -> length x >= n) $ byValue xs

bySuit :: [Card] -> [[Card]]
bySuit xs = [ [ x | x <- xs, suit x == s ] | s <- [Clubs .. Spades] ]

byValue :: [Card] -> [[Card]]
byValue xs = reverse [ [ x | x <- xs, value x == v ] | v <- [Two .. Ace] ]

valueOrHigher :: Value -> Card -> Bool
valueOrHigher v c = value c >= v
