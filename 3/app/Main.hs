module Main where

import Data.Char
import Data.List

main :: IO ()
main = do
 print . sum . map maxJolt . lines =<< readFile "./example"
 print . sum . map maxJolt . lines =<< readFile "./puzzle"
 -- Naive bruteforce, the length of an individual bank is 100, so the
 -- subsequenceis a list of 100! numbers (yikes)
 print . sum . map (maximum . map read . filter (\x -> length x == 12) . subsequences :: String -> Integer) . lines =<< readFile "./example"

maxJolt :: [Char] -> Int
maxJolt [] = -1
maxJolt [x] = digitToInt x
maxJolt (x:xs) = max (digitToInt x * 10 + digitToInt (maximum xs)) (maxJolt xs)
