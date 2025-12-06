module Main where

import Data.List

main :: IO ()
main = do
    print . part1 =<< readFile "./example"
    print . part1 =<< readFile "./puzzle"
    print . part2 =<< readFile "./example"
    print . part2 =<< readFile "./puzzle"

part1 :: String -> Int
part1 = sum . map (homeWork . reverse) . transpose . map words . lines
part2 :: String -> Int
part2 s = sum . map solveProblem . init . splitProblems (map length . splitOperators . last . lines $ s) . reverse . lines $ s

homeWork :: [String] -> Int
homeWork [] = 0
homeWork (x:xs)
    | x == "*" = product . map read $ xs
    | x == "+" = sum . map read $ xs
    | otherwise = 0

solveProblem :: [String] -> Int
solveProblem p = homeWork ((concat . words . head $ p) : (transpose . reverse . tail $ p))

splitProblems :: [Int] -> [String] -> [[String]]
splitProblems (x:xs) y = map (take x) y : (splitProblems xs . map (drop (x + 1))) y
splitProblems _ _ = [[]]

splitOperators :: String -> [String]
splitOperators = concatGroupedOperators . group
    where
        concatGroupedOperators [x, y] = [x ++ y]
        concatGroupedOperators (x:y:xs) = (x ++ init y) : concatGroupedOperators xs
        concatGroupedOperators _ = []
