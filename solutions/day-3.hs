-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

module Main where

import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map as Map

-- solve1 :: [String] -> Integer
-- solve1 = sum . map validGame

-- solve2 :: [String] -> Integer
-- solve2 = sum . map cubePower

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath >>= return . lines
    -- putStr "part 1 solution: " >> print (solve1 input)
    -- putStr "part 2 solution: " >> print (solve2 input)
