module Main where

import Control.Monad.Random
import Data.List
import Data.Maybe
import Test.QuickCheck
import InOut
import Risk

prop_readCorrectly1 :: [Maybe Int] -> Bool
prop_readCorrectly1 xs = readCorrectly xs == False
                    where xs = [(Just 1),(Just 2),(Just 3),(Nothing)]

prop_readCorrectly2 :: [Maybe Int] -> Bool
prop_readCorrectly2 xs = readCorrectly xs == True
                    where xs = [(Just 1),(Just 2),(Just 3)]

prop_readCorrectly3 :: [Maybe Int] -> Bool
prop_readCorrectly3 xs = readCorrectly xs == False
                    where xs = [(Nothing)]

prop_readCorrectly4 :: [Maybe Int] -> Bool
prop_readCorrectly4 xs = readCorrectly xs == True
                    where xs = []

prop_readCorrectly5 :: [Maybe Int] -> Bool
prop_readCorrectly5 xs = readCorrectly xs == False
                    where xs = [(Just 1),(Just 2),(Nothing),(Just 3)]



prop_correctArg1 :: String -> Bool
prop_correctArg1 xs = correctArg xs == True
         where xs = "2134 23442"

prop_correctArg2 :: String -> Bool
prop_correctArg2 xs = correctArg xs == False
         where xs = "1.9 657"

prop_correctArg3 :: String -> Bool
prop_correctArg3 xs = correctArg xs == False
         where xs = " p p p qq"

prop_correctArg4 :: String -> Bool
prop_correctArg4 xs = correctArg xs == False
         where xs = "10 2 56"

prop_correctArg5 :: String -> Bool
prop_correctArg5 xs = correctArg xs == False
         where xs = ""

{-
prop_first1 :: (Int -> Int) -> (Int, Int) -> Bool
prop_first1
-}
{-
prop_die :: Bool
prop_die =
-}








main :: IO ()
main = do quickCheck prop_readCorrectly1
          quickCheck prop_readCorrectly2
          quickCheck prop_readCorrectly3
          quickCheck prop_readCorrectly4
          quickCheck prop_readCorrectly5
          quickCheck prop_correctArg1
          quickCheck prop_correctArg2
          quickCheck prop_correctArg3
          quickCheck prop_correctArg4
          quickCheck prop_correctArg5
          --quickCheck prop_first1
          --quickCheck prop_die
