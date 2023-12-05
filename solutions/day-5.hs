-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

-- Approach: turn each "map" into a function of functions.
--
-- e.g.: 
-- seed-to-soil map:
-- 50 98 2
-- 52 50 48
--
-- is really just f (g . h) (x) where:
--   g(x) = mapping from x in range [98 .. 99] to domain [50 .. 51], -1 otherwise
--   h(x) = mapping from x in range [50 .. 97] to domain [52 .. 99], -1 otherwise
--   f(x) = max (g(x), h(x), x)  

module Main where

import System.Environment (getArgs)
import Data.List.Split

type CategoryMap = [Int -> Int]

noMapVal :: Int
noMapVal = -1

-- Turns map entries into a function mapping source categories to destination categories.
applyMapRange :: [Int] -> Int -> Int
applyMapRange [dst, src, len] x = if x >= src && x <= src + len - 1 then dst + (x - src) else noMapVal
applyMapRange _ _ = -1

-- Creates list of maps (lists of functions) from input.
createMaps :: [String] -> [CategoryMap]
createMaps input = map (map applyMapRange) $ map parseRanges input
    where parseRanges = map (map read . splitOn " ") . lines . last . splitOn "map:\n"

-- Given a source int and a list of functions comprising a category map,
-- apply the source int to the map and return the destination int.
mapLookup :: Int -> CategoryMap -> Int
mapLookup x mapRanges =
    let lkup = maximum $ map ($ x) mapRanges
    in if lkup /= noMapVal then lkup else x

-- Pipeline seed through each map.
seedLookup :: Int -> [CategoryMap] -> Int
seedLookup s maps = compose mapFns s
    where mapFns        = [(`mapLookup` m) | m <- maps]
          compose fs v  = foldl (flip (.)) id fs $ v

solve1 :: [String] -> Int
solve1 input = 
    let seedList = map read $ splitOn " " $ last $ splitOn "seeds: " $ head input :: [Int]
        maps     = createMaps $ tail input
    in minimum $ map (`seedLookup` maps) seedList

-- Converts list into list of tuples.
-- Only works with even-lengthed lists ... ¯\_(ツ)_/¯
pack :: [Int] -> [(Int, Int)]
pack [] = []
pack (x:xs) = [(x, if (length xs) > 0 then head xs else -1)] ++ pack (tail xs)

solve2 :: [String] -> Int
solve2 input = 
    let seedRanges = map read $ splitOn " " $ last $ splitOn "seeds: " $ head input :: [Int]
        rangePairs = pack seedRanges
        seedList   = concat [[x .. (x + y - 1)] | (x, y) <- rangePairs]
        maps       = createMaps $ tail input
    in minimum $ map (`seedLookup` maps) seedList

main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath >>= return . splitOn "\n\n"
    putStr "part 1 solution: " >> print (solve1 input)
    putStr "part 2 solution: " >> print (solve2 input)
