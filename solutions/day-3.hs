-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

module Main where

import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List.Split
import Data.List (nubBy)

type Point = (Int, String)
type Grid = [[Point]]

splitLine :: String -> [String]
splitLine = split (dropInitBlank $ dropInnerBlanks $ dropFinalBlank $ whenElt (not . isDigit))

addIdAndFlatten :: [[String]] -> [Point]
addIdAndFlatten = zip [0..] . concat

expandToGrid :: Int -> [Point] -> Grid
expandToGrid listSize tuples = chunksOf listSize $ concat
    [map (\z -> (y, z)) (take (length x) (repeat x)) | (y, x) <- tuples]

getGridPoint :: Grid -> (Int, Int) -> Point
getGridPoint grid (row, col) = grid !! row !! col

inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds h w (x, y) = if x < 0 || y < 0 || x >= h || y >= w then False else True

gridNeighbors :: Int -> Int -> (Int, Int) -> [(Int, Int)]
gridNeighbors h w (x, y) = filter (inBounds h w) [n1, n2, n3, n4, n5, n6, n7, n8]
    where n1 = (x, y+1)     -- right
          n2 = (x+1, y)     -- down
          n3 = (x, y-1)     -- left
          n4 = (x-1, y)     -- up
          n5 = (x-1, y-1)   -- up-left
          n6 = (x-1, y+1)   -- up-right
          n7 = (x+1, y-1)   -- down-left
          n8 = (x+1, y+1)   -- down-right

isSymbol :: String -> Bool
isSymbol str = length str == 1 && (not $ isDigit $ head str) && str /= "."

isSymbolAdjacent :: [String] -> Bool
isSymbolAdjacent = (> 0) . length . filter (isSymbol)

dedupe :: Point -> Point -> Bool
dedupe (a,_) (b,_) = a == b

solve1 :: [String] -> Integer
solve1 input =
    let rows = length input
        cols = length (input !! 0)
        grid = expandToGrid cols $ addIdAndFlatten $ map splitLine input
        idxs = concat [[(r, c) | c <- [0..cols-1]] | r <- [0..rows-1]]
        neighborIdxs = map (gridNeighbors rows cols) idxs
        neighborPoints = map (map (getGridPoint grid)) neighborIdxs
        neighborChars = (map (map (\(_,y) -> y)) neighborPoints)
        symbAdjacentMask = map isSymbolAdjacent neighborChars
        symbAdjacentPoints = filter (isDigit . head . snd) $ map fst $ filter (snd) $ zip (concat grid) symbAdjacentMask
        deduped = nubBy dedupe symbAdjacentPoints
    in sum $ map (read . snd) deduped

solve2 :: [String] -> Integer
solve2 input =
    let rows = length input
        cols = length (input !! 0)
        grid = expandToGrid cols $ addIdAndFlatten $ map splitLine input
        idxs = concat [[(r, c) | c <- [0..cols-1]] | r <- [0..rows-1]]
        neighborIdxs = map (gridNeighbors rows cols) idxs
        neighborPoints = map (map (getGridPoint grid)) neighborIdxs
        neighborChars = map snd $ filter ((=="*") . snd . fst) $ zip (concat grid) neighborPoints
        dedupedNums = map ((map snd) . nubBy dedupe . filter (isDigit . head . snd)) neighborChars
        gears = map (map read) $ filter ((==2) . length) dedupedNums
    in sum $ map product gears

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath >>= return . lines
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
