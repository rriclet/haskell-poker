module Main where

import Card
import Common
import Deal
import Deck

main :: IO ()
main = print "Coucou"
{-
main = flip dealAllHole [p1,p2,p3] <$> deck
     where p1 = Player "Rob" 50
           p2 = Player "Lea" 50
           p3 = Player "Clm" 50
-}