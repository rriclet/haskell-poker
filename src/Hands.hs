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
  | fiveCards rf = (RoyalFlush, rf)
  | fiveCards sf = (StraightFlush, sf)
  | fiveCards fk = (FourOfKind, fk)
  | fiveCards fh = (FullHouse, fh)
  | fiveCards fl = (Flush, fl)
  | fiveCards st = (Straight, st)
  | fiveCards tk = (ThreeOfKind, tk)
  | fiveCards tp = (TwoPair, tp)
  | fiveCards op = (OnePair, op)
  | otherwise      = (HighCard, hc)
    where fiveCards x = length x == 5 
          rf  = royalFlush xs
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
royalFlush = take 5 . sortOn value . concat . filter (\x -> length x == 5) . map (filter $ valueOrHigher Ten) . bySuit

straightFlush :: [Card] -> [Card]
straightFlush = straight' True 

fourOfKind :: [Card] -> [Card]
fourOfKind xs = fst fkAndLeft ++ highCard 1 (snd fkAndLeft)
              where fkAndLeft = handAndCardsLeft 4 xs

fullHouse :: [Card] -> [Card]
fullHouse xs = fst (threeOfKind' xs) ++ fst (onePair' (snd $ threeOfKind' xs))

flush :: [Card] -> [Card]
flush = take 5 . concat . sortOn (Down . length) . filter (\x -> length x >= 5) . bySuit . sortOn (Down . value)

straight :: [Card] -> [Card]
straight = straight' False

straight' :: Bool -> [Card] -> [Card]
straight' flush = foldl straightFold [] . copyAces .  sortOn (Down . value)
  where straightFold l c
          | null l                  = [c]
          | ended                   = l
          | same value && not flush = l
          | same value && flush     = l
          | addCard                 = c : l
          | otherwise               = [c]
          where last           = head l
                same f         = f c == f last
                ended          = length l == 5
                addCard        = (canAdd && not flush) || (canAddFlush && flush)  
                canAdd         = isSucc || canAddAce 
                canAddFlush    = canAdd && same suit
                isSucc         = not isAce && succ (value c) == value last
                canAddAce      = isAce && length l == 4 && value last == Two
                isAce          = value c == Ace  

-- we copy Ace's to the end to get Straight's like A2345
copyAces :: [Card] -> [Card]
copyAces xs = xs ++ filter (\x -> value x == Ace) xs

threeOfKind :: [Card] -> [Card]
threeOfKind xs = fst (threeOfKind' xs) ++ highCard 2 (snd $ threeOfKind' xs)
threeOfKind' :: [Card] -> ([Card], [Card])
threeOfKind' = handAndCardsLeft 3

twoPair :: [Card] -> [Card]
twoPair xs = fst firstPair ++ fst secondPair ++ highCard 1 (snd secondPair)
              where firstPair  = onePair' xs
                    secondPair = onePair' (snd firstPair)

onePair :: [Card] -> [Card]
onePair xs = fst (onePair' xs) ++ highCard 3 (snd $ onePair' xs)
onePair' :: [Card] -> ([Card], [Card])
onePair' = handAndCardsLeft 2

highCard :: Int -> [Card] -> [Card]
highCard n = take n . sortOn (Down . value) 

handAndCardsLeft :: Int -> [Card] -> ([Card], [Card])
handAndCardsLeft n xs = (hand, xs \\ hand) 
                          where hand = take n $ concat $ filter (\x -> length x >= n) $ byValue xs

bySuit :: [Card] -> [[Card]]
bySuit xs = [ [ x | x <- xs, suit x == s ] | s <- [Clubs .. Spades] ]

byValue :: [Card] -> [[Card]]
byValue xs = reverse [ [ x | x <- xs, value x == v ] | v <- [Two .. Ace] ]

valueOrHigher :: Value -> Card -> Bool
valueOrHigher v c = value c >= v
