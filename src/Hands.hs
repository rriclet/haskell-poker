module Hands ( bestHand
             , royalFlush
             , handAndCardsLeft
             , straight
             , straightFold
             , copyAces
             , highCard
             , bySuit
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
  -- Straight Flush
  | length fk == 5 = (FourOfKind, fk)
  | length fh == 5 = (FullHouse, fh)
  -- Flush
  | length st == 5 = (Straight, st)
  | length tk == 5 = (ThreeOfKind, tk)
  | length tp == 5 = (TwoPair, tp)
  | length op == 5 = (OnePair, op)
  | otherwise      = (HighCard, hc)
    where rf  = royalFlush xs
          fk  = fst (partial 4) ++ highCard 1 (snd $ partial 4)
          fh  = fst tk' ++ fst (handAndCardsLeft (snd tk') 2)
          st  = straight xs
          tk  = fst tk' ++ highCard 2 (snd tk')
          tk' = partial 3
          tp  = fst op' ++ fst tp' ++ highCard 1 (snd tp')
          tp' = handAndCardsLeft (snd op') 2
          op  = fst op' ++ highCard 3 (snd op')
          op' = partial 2
          hc  = highCard 5 xs
          partial = handAndCardsLeft xs

royalFlush :: [Card] -> [Card]
royalFlush = take 5 . concat . filter (\x -> length x == 5) . map (filter $ valueOrHigher Ten) . bySuit

handAndCardsLeft :: [Card] -> Int -> ([Card], [Card])
handAndCardsLeft xs n = (hand, xs \\ hand) 
                          where hand = take n $ concat $ filter (\x -> length x >= n) $ byValue xs

straight :: [Card] -> [Card]
straight = foldl straightFold [] . copyAces .  sortOn (Down . value)

-- we copy Ace's to the end to get Straight's like A2345
copyAces :: [Card] -> [Card]
copyAces xs = xs ++ filter (\x -> value x == Ace) xs

straightFold :: [Card] -> Card -> [Card]
straightFold l c
  | null l              = [c]
  | isSameOrEnded       = l
  | isSucc || canAddAce = c : l
  | otherwise           = [c]
  where lastValue     = value (head l)
        isSameOrEnded = value c == lastValue || length l == 5
        isAce         = value c == Ace  
        isSucc        = not isAce && succ (value c) == lastValue
        canAddAce     = isAce && length l == 4 && lastValue == Two

highCard :: Int -> [Card] -> [Card]
highCard n = take n . sortOn (Down . value) 

bySuit :: [Card] -> [[Card]]
bySuit xs = [ [ x | x <- xs, suit x == s ] | s <- [Clubs .. Spades] ]

byValue :: [Card] -> [[Card]]
byValue xs = reverse [ [ x | x <- xs, value x == v ] | v <- [Two .. Ace] ]

valueOrHigher :: Value -> Card -> Bool
valueOrHigher v c = value c >= v
