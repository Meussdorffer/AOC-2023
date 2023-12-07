module Main where

import System.Environment (getArgs)
import Data.List.Split

solve1 :: Int
solve1 = 1

solve2 :: Int
solve2 = 2

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
