module CardParser 
( p
) where

import Card
import Control.Monad
import Data.Maybe

p :: [String] -> [Card]
p = mapMaybe toCard 

toCard :: String -> Maybe Card
toCard (x1:x2:_) = liftM2 Card (readValue x1) (readSuit x2)
toCard _         = Nothing

readValue :: Char -> Maybe Value
readValue x = case x of
                '2' -> Just Two
                '3' -> Just Three
                '4' -> Just Four
                '5' -> Just Five
                '6' -> Just Six
                '7' -> Just Seven
                '8' -> Just Eight
                '9' -> Just Nine
                'T' -> Just Ten
                'J' -> Just Jack
                'Q' -> Just Queen
                'K' -> Just King
                'A' -> Just Ace
                _   -> Nothing

readSuit :: Char -> Maybe Suit
readSuit x = case x of
                'c' -> Just Clubs
                'd' -> Just Diamonds
                'h' -> Just Hearts
                's' -> Just Spades
                _   -> Nothing
