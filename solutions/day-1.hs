-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

module Main where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List

firstAndLast :: String -> String
firstAndLast [x] = [x] ++ [x]
firstAndLast xs = [head xs, last xs]

solve1 :: String -> Integer
solve1 = sum . map (read . firstAndLast . filter isDigit) . lines

numbers :: [String]
numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

substrToNum :: String -> String
substrToNum str = maybe (if isDigit (head str) then [head str] else "") show $ 
                  findIndex (`isPrefixOf` str) numbers

parseString :: String -> String
parseString = concatMap substrToNum . filter (/= "") . tails

solve2 :: String -> Integer
solve2 = sum . map (read . firstAndLast . parseString) . lines

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
