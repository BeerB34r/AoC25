{-# LANGUAGE TupleSections #-}
module Main where

import Data.List
import Data.MemoTrie

main :: IO ()
main = do
    print . part1 =<< readFile "./example"
    print . part1 =<< readFile "./puzzle"
    print . part2 =<< readFile "./example"
    print . part2 =<< readFile "./puzzle"

part1 :: String -> Int
part1 = length . concatMap (filter (=='v')) . tachyonBeams . lines . map (\x -> if x == 'S' then '|' else x)

part2 :: String -> Int
part2 s = timeyWimey (map (1,) . elemIndices 'S' . head . lines $ s) . tail . lines $ s

timeyWimey :: [(Int,Int)] -> [String] -> Int
timeyWimey a [] = sum . map fst $ a
timeyWimey a (x:xs) = timeyWimey (enterMatrix a x) xs

enterMatrix :: [(Int,Int)] -> String -> [(Int,Int)]
enterMatrix [] _ = []
enterMatrix is s = map (foldl (\(a,_) (c,b) -> (a + c,b)) (0,0)) . group . sort . concatMap (\(a,b) -> map (a,) $ checkSplit s b) $ is

checkSplit :: String -> Int -> [Int]
checkSplit = memo _checkSplit
    where
        _checkSplit :: String -> Int -> [Int]
        _checkSplit [] _ = []
        _checkSplit s i = if s !! i == '^' then [i - 1, i + 1] else [i]

-- TOO FUCKING SLOW
followTachyonPath :: (Int,[String]) -> Int
followTachyonPath (_, []) = 1
followTachyonPath (i, x:xs)
    | x !! i == '^' = followTachyonPath (i-1, xs) + followTachyonPath (i+1, xs)
    | x !! i /= '^' = followTachyonPath (i, xs)
    | otherwise = 0

tachyonBeams :: [String] -> [String]
tachyonBeams [] = []
tachyonBeams s
    | (splitBeams . transpose . propogateBeams . transpose $ s) == s = s
    | otherwise = tachyonBeams . splitBeams . transpose . propogateBeams . transpose $ s

propogateBeams :: [String] -> [String]
propogateBeams [] = []
propogateBeams (x:xs)
    | '|' `elem` x = foldl (\a b -> if null a then [b] else if b == '^' && last a == '|' then a ++ "x" else if b == '.' && last a == '|' then a ++ "|" else a ++ [b]) [] x : propogateBeams xs
    | otherwise =  x : propogateBeams xs

splitBeams :: [String] -> [String]
splitBeams [] = []
splitBeams (x:xs)
    | 'x' `elem` x = splitBeam x : splitBeams xs
    | otherwise = x : splitBeams xs

splitBeam :: String -> String
splitBeam s
    | 'x' `elem` s = do
        let (prefix,suffix) = break (=='x') s
        splitBeam $ init prefix ++ "|v|" ++ drop 2 suffix
    | otherwise = s
