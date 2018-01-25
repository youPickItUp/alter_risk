module Main where

import Data.Maybe
import Text.Read
import Risk

-- | Function checks if readMaybe function returned wrapped Int values. If list contain Nothing returns False otherwise, True.
readCorrectly :: [Maybe a] -> Bool
readCorrectly [] = True
readCorrectly (x:xs) = if isNothing x then False
                       else readCorrectly xs

-- | Function checks if input matches expected format - two nonnegative Ints. 
correctArg :: String -> Bool
correctArg [] = False
correctArg argLine = let wordsList = words argLine in
                     if (length wordsList) /= 2 then False
                     else
                        let twoIntsMaybe = map readMaybe wordsList :: [Maybe Int] in
                        if not $ readCorrectly twoIntsMaybe then False
                           else
                             let a = fromJust $ head twoIntsMaybe in
                             let b = fromJust $ last twoIntsMaybe in
                             if a<0 || b<0 then False
                             else True




-- | Passes an arguments and prints the result - approximated odds of annihilation of defending army.
main :: IO ()
main = do
    line <- getLine
    if correctArg line then do
      let a = head ((map read (words line)) :: [Int])
      let b = last ((map read (words line)) :: [Int])
      x <- showOutcome $ successProb $ Battlefield a b
      print x
    else do
      putStrLn "Należy podać dwa argumenty (Int) - wielkości odpowiednio armii atakującej i broniącej."
      main
