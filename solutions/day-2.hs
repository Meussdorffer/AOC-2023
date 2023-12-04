-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

module Main where

import System.Environment (getArgs)
import Data.List.Split
import qualified Data.Map as Map

gameId :: String -> Integer
gameId = read . last . words . head . splitOn ":"

parsePair :: [String] -> (Integer, String)
parsePair [x, y] = (read x, y)
parsePair _      = (0, "")

gameSets :: String -> [(Integer, String)]
gameSets gamestr = 
    let parts = splitOn ": " gamestr
        setstr = last parts
        setGroups = splitOn "; " setstr
        parseGroup grp = map (parsePair . splitOn " ") (splitOn ", " grp)
    in concatMap parseGroup setGroups

-- Returns True if pair is valid based on possible cube quantities, else False.
pairCheck :: (Integer, String) -> Bool
pairCheck (x, y)
    | y == "red"    = x <= 12
    | y == "green"  = x <= 13
    | y == "blue"   = x <= 14
    | otherwise     = False

-- Returns the game ID if game is valid, else 0.
validGame :: String -> Integer
validGame str = 
    let gid = gameId str
        pairs = gameSets str
        invalidPairs = filter (not . pairCheck) pairs
    in if (length invalidPairs) > 0 then 0 else gid

solve1 :: [String] -> Integer
solve1 = sum . map validGame

cubePower :: String -> Integer
cubePower str =
    let pairs = gameSets str
        grpByColor = Map.toList $ Map.fromListWith (++) $ map (\(v,k) -> (k,[v])) pairs
        maxQtys = map (maximum . snd) grpByColor
    in product maxQtys

solve2 :: [String] -> Integer
solve2 = sum . map cubePower

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath >>= return . lines
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
