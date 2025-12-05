module Main where

import Data.List

main :: IO ()
main = do
    print . part1 =<< readFile "./example"
    print . part1 =<< readFile "./puzzle"
    print . part2 =<< readFile "./example"
    print . part2 =<< readFile "./puzzle"

part1 :: String -> Int
part1 s = length . filter (\x -> any (\(a,b) -> a <= x && x <= b) . getInventory $ s) . map read . tail . dropWhile (not . null) . lines $ s

part2 :: String -> Integer
part2 = sum . map (\(a,b) -> b - a + 1) . recursivelyCombine . getInventory

recursivelyCombine :: [(Integer,Integer)] -> [(Integer,Integer)]
recursivelyCombine x = do
    let folded = map head . group . sort . foldl potentiallyCombine [] $ x
    if x == folded then x else recursivelyCombine folded

potentiallyCombine :: [(Integer,Integer)] -> (Integer,Integer) -> [(Integer,Integer)]
potentiallyCombine [] x = [x]
potentiallyCombine [(a,b)] (c,d)
    | c <= a && a <= d = if b >= d then [(c,b)] else [(c,d)]
    | a <= c && c <= b = if d >= b then [(a,d)] else [(a,b)]
potentiallyCombine (x:xs) y = x : potentiallyCombine xs y

getInventory :: String -> [(Integer,Integer)]
getInventory s = zip (map (read . takeWhile (/= '-')) . takeWhile (not . null) . lines $ s) (map (read . tail . dropWhile (/= '-')) . takeWhile (not . null) . lines $ s)
