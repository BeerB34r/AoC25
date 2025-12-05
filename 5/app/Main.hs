module Main where

import Data.List

main :: IO ()
main = do
    print . part1 =<< readFile "./example"
    print . part1 =<< readFile "./puzzle"
    print . part2Bad =<< readFile "./example"
    let ranges = [(3,5),(10,14),(16,20),(12,18)] ::[(Int,Int)]
    let foldedRanges = foldl potentiallyCombine [] ranges
    print $ potentiallyCombine [(10,14)] (12,20)
    print ranges
    print foldedRanges
    -- print . part2 =<< readFile "./puzzle"

part1 :: String -> Int
part1 s = length . filter (\x -> any (\(a,b) -> a <= x && x <= b) . getInventory $ s) . map read . tail . dropWhile (not . null) . lines $ s

potentiallyCombine :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
potentiallyCombine [] x = [x]
potentiallyCombine [(a,b)] (c,d)
    | c <= a && a <= d = if b >= d then [(c,b)] else [(c,d)]
    | a <= c && c <= b = if d >= b then [(a,d)] else [(a,b)]
potentiallyCombine (x:xs) y = x : potentiallyCombine xs y

getInventory :: String -> [(Int,Int)]
getInventory s = zip (map (read . takeWhile (/= '-')) . takeWhile (not . null) . lines $ s) (map (read . tail . dropWhile (/= '-')) . takeWhile (not . null) . lines $ s)

-- BAD, time complexity so bad the computer will grind to a screeching halt
getInventoryBad :: String -> [Int]
getInventoryBad = (\x -> map head . group . sort . concat $ zipWith (\a b -> [a..b]) (map ((read::String->Int) . takeWhile (/='-')) x) (map (read . tail . dropWhile (/='-')) x)) . takeWhile (not . null) . lines

-- BAD, same shit as with getInventoryBad
part2Bad :: String -> Int
part2Bad s = do
    let inventory = getInventory s
    let fullInventory = map head . group . sort . concatMap (\(a,b) -> [a..b]) $ inventory ::[Int]
    length fullInventory
