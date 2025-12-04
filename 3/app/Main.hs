module Main where

import Data.Char

main :: IO ()
main = do
 print . sum . map maxJolt . lines =<< readFile "./example"
 print . sum . map maxJolt . lines =<< readFile "./puzzle"
 print . sum . map maxUJolt . lines =<< readFile "./example"
 print . sum . map maxUJolt . lines =<< readFile "./puzzle"

maxJolt :: [Char] -> Int
maxJolt [] = -1
maxJolt [x] = digitToInt x
maxJolt (x:xs) = max (digitToInt x * 10 + digitToInt (maximum xs)) (maxJolt xs)

maxUJolt :: [Char] -> Int
maxUJolt [] = -1
maxUJolt x = read $ foldl (incrementJolt) (take 12 x) (drop 12 x)

incrementJolt :: [Char] -> Char -> [Char]
incrementJolt x y = maximum $ (map (++ [y]) . removeAny $ x) ++ [x]

removeAny :: [Char] -> [[Char]]
removeAny x = map (\(a,b) -> a ++ drop 1 b) . zipWith (\a b -> splitAt a b) [0..] . take (length x) . cycle $ [x]
