{-
Approach:

Combination of race strategies can be represented as a matrix,
with rows = number of button presses, and columns = timestep t in range [1, T].

E.g. T=7

t : 1   2   3   4   5   6   7  -> total
----------------------------------------
x1 [0   1   1   1   1   1   1] -> 6
x2 [0   0   2   2   2   2   2] -> 10
x3 [0   0   0   3   3   3   3] -> 12
x4 [0   0   0   0   4   4   4] -> 12
x5 [0   0   0   0   0   5   5] -> 10
x6 [0   0   0   0   0   0   6] -> 6

Here, there are T timesteps and S=T-1 strategies that result in positive distance.
Given the above, the total distance raced over time T for any given strategy s in range [1, T-1]
(where s is the number of button presses) can be directly calculated as such:

f(t, s) = (t - s) * s

Applying this function over a range of strategies S = [1, T-1] returns the race distance for every possible strategy. 
-}

module Main where

import System.Environment (getArgs)
import Data.List.Split

numWinningStrategies :: Int -> Int -> Int
numWinningStrategies time dist = length $ filter (> dist) $ raceDistances
    where raceDistances = [(time - presses) * presses | presses <- [1..time-1]]

solve1 :: String -> Int
solve1 input =
    let subListSplit = split (dropInitBlank $ dropInnerBlanks $ dropDelims $ oneOf " ")
        parsed = map (map (read) . subListSplit . last . splitOn ":") . lines $ input
        zipped = zip (head parsed) (last parsed)
    in product $ [numWinningStrategies time dist | (time, dist) <- zipped]

solve2 :: String -> Int
solve2 input =
    let parsed = map (read . concat . splitOn " " . last . splitOn ":") . lines $ input
        (time, dist) = (head parsed, last parsed)
    in numWinningStrategies time dist

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
