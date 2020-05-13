module Main where

import qualified Test.Deal as Deal
import qualified Test.Hands as Hands
import System.Exit

main :: IO Bool
main = do 
  -- add test runners into the array for each module
  good <- and <$> sequence [Deal.runTests, Hands.runTests]
  if good
     then exitSuccess
     else exitFailure
