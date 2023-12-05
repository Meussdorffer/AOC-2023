-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

module Main where

import System.Environment (getArgs)
import Data.List.Split


parseNums :: String -> ([Int], [Int])
parseNums input =
    let [x, y] = map (map read . filter (/="") . splitOn " ") . splitOn " | " . last . splitOn ": " $ input
    in (x, y)

numWinningNums :: ([Int], [Int]) -> Int
numWinningNums (win, mine) = length $ filter (`elem` win) mine

points :: Int -> Int
points x
    | x > 0     = 2 ^ (x - 1)
    | otherwise = 0


solve1 :: [String] -> Int
solve1 = sum . map (points . numWinningNums . parseNums) 

winningCards :: [Int] -> [Int]
winningCards [] = []
winningCards (x:xs) = 1 + sum (take x rst) : rst
    where rst = winningCards xs

solve2 :: [String] -> Int
solve2 = sum . winningCards . map (numWinningNums . parseNums) 

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath >>= return . lines
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
