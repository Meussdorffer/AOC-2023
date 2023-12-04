-- Run this with cabal run -- day-x <part-number> <file-to-solution>
-- Example: cabal run -- day-3 2 "./input/day-3.example"

module Main where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List

firstAndLast :: String -> String
firstAndLast [x] = [x] ++ [x]
firstAndLast xs = [head xs] ++ [last xs]

solve1 :: String -> Integer
solve1 = sum . map (read . firstAndLast . filter isDigit) . lines

numbers :: [String]
numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

substrToNum :: String -> String
substrToNum str = case elemIndex True (map (\x -> isPrefixOf x str) numbers) of
    Just n -> show n
    Nothing -> if isDigit (head str) then [(head str)] else ""

parseString :: String -> String
parseString = intercalate "" . map (substrToNum) . filter (/= "") . tails

solve2 :: String -> Integer
solve2 = sum . map (read . firstAndLast . parseString) . lines

main :: IO ()
main = do
    [part, filepath] <- getArgs
    input <- readFile filepath
    if read @Int part == 1
    then do
        putStrLn "solution to problem 1 is:"
        print $ solve1 input
    else do
        putStrLn "solution to problem 2 is:"
        print $ solve2 input
