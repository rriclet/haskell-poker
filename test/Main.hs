module Main where

import qualified Test.Hands as Hands
import System.Exit

main :: IO Bool
main = do 
  -- add test runners into the array for each module
  good <- and <$> sequence [Hands.runTests]
  if good
     then exitSuccess
     else exitFailure
