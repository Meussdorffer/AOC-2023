-- Run this with cabal run -- day-x "input filepath"
-- Example: cabal run -- day-1 "./input/day-1.example"

module Main where

import System.Environment (getArgs)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

-- Convert x-to-y map string into a Haskell Map to streamline lookups.
strmapToMap :: String -> Map.Map Int Int
strmapToMap = Map.fromList . concatMap (expandRange) . parseRanges
    where parseRanges = map (map read . splitOn " ") . lines . last . splitOn "map:\n"
          expandRange [dst, src, len] = [(src + n, dst + n) | n <- [0 .. len - 1]]

mapLookup :: Int -> Map.Map Int Int -> Int
mapLookup k m = if Map.member k m then m Map.! k else k

-- Apply initial seed to list of map functions resulting in a lookup number from the final map
seedLookup :: Int -> [Map.Map Int Int] -> Int
seedLookup s maps = compose mapFns s
    where mapFns        = [(`mapLookup` m) | m <- maps]
          compose fs v  = foldl (flip (.)) id fs $ v



solve1 :: [String] -> Int
solve1 input = 
    let seedList = map read $ splitOn " " $ last $ splitOn "seeds: " $ head input :: [Int]
        maps = map strmapToMap $ tail input
    in minimum $ map (`seedLookup` maps) seedList

-- solve2 :: [String] -> Int
-- solve2 = sum . winningCards . map (numWinningNums . parseNums) 


main :: IO ()
main = do
    [filepath] <- getArgs
    input <- readFile filepath >>= return . splitOn "\n\n"
    putStr "part 1 solution: " >> print (solve1 input)
    -- putStr "part 2 solution: " >> print (solve2 input)

    -- let seedList = map read $ splitOn " " $ last $ splitOn "seeds: " $ head input :: [Int]
    --     maps = map strmapToMap $ tail input
    --     mls = [(`mapLookup` m) | m <- maps]

    -- print $ map (`seedLookup` maps) seedList
    -- print $ (`seedLookup` maps) 79
    -- print $ foldr mapLookup 79 maps
    -- print $ (`mapLookup` (maps !! 0)) 79
    -- print $ compose (map `mapLookup` maps) 79
    -- print $ compose mls 79
    -- print $ foldl mls 79
    -- print mls
