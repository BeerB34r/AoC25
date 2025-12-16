module Main where

import Data.List
import Data.Ord
import qualified Data.Bifunctor
import GHC.Float

type Vertex = (Float,Float,Float)
getX :: Vertex -> Float
getX (x,_,_) = x
getY :: Vertex -> Float
getY (_,y,_) = y
getZ :: Vertex -> Float
getZ (_,_,z) = z

main :: IO ()
main = do
    print . (`part1` 10) =<< readFile "./example"
    print . (`part1` 1000) =<< readFile "./puzzle"
    print . part2 =<< readFile "./example"
    print . part2 =<< readFile "./puzzle"

part1 :: String -> Int -> Int
part1 s n = do
    let input = map ((\x -> (head x, x !! 1, x !! 2)) . map read . splitOn ',') $ lines s ::[Vertex]
    let distances = map head . group . sort . map (\(a,b,c) -> if b < c then (a,b,c) else (a,c,b)) . concatMap ((\y -> map (\x -> (fst x, snd x, snd y)) . fst $ y) . (\x -> (x `getDistances` input, x))) $ input ::[(Float,Vertex,Vertex)]
    product
        .take 3
        .sortBy (comparing Down)
        .map length
        .createGroups (map (:[]) input)
        .map (\(_,a,b) -> (a,b))
        .take n $ distances

part2 :: String -> Int
part2 s = do
    let input = map ((\x -> (head x, x !! 1, x !! 2)) . map read . splitOn ',') $ lines s ::[Vertex]
    let distances = map head . group . sort . map (\(a,b,c) -> if b < c then (a,b,c) else (a,c,b)) . concatMap ((\y -> map (\x -> (fst x, snd x, snd y)) . fst $ y) . (\x -> (x `getDistances` input, x))) $ input ::[(Float,Vertex,Vertex)]
    uncurry (*)
        .Data.Bifunctor.bimap (float2Int . getX) (float2Int . getX) $
            connectCircuit (map (:[]) input) (map (\(_,a,b) -> (a,b)) distances)


connectCircuit :: [[Vertex]] -> [(Vertex, Vertex)] -> (Vertex,Vertex)
connectCircuit _ [] = ((0,0,0),(0,0,0))
connectCircuit vs (x:xs) = if (length . createGroups vs) [x] == 1 then x else connectCircuit (createGroups vs [x]) xs

createGroups :: [[Vertex]] -> [(Vertex, Vertex)] -> [[Vertex]]
createGroups vs [] = vs
createGroups vs (x:xs) = do
    let (first,second) = x
    let firstGroup = concat $ filter (first `elem`) vs
    let secondGroup = concat $ filter (second `elem`) vs
    if firstGroup /= secondGroup then do
        let rest = filter (\y -> not ((first `elem` y) || (second `elem` y))) vs
        let newGroup = [firstGroup ++ secondGroup]
        createGroups (rest ++ newGroup) xs
    else
        createGroups vs xs


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = if c `elem` s then takeWhile (/=c) s : (splitOn c . tail . dropWhile (/=c) $ s) else [s]

getDistances :: Vertex -> [Vertex] -> [(Float,Vertex)]
getDistances _ [] = []
getDistances v s = tail . sort . map (\x -> (distance v x, x)) $ s

distance :: Vertex -> Vertex -> Float
distance (a,b,c) (x,y,z) = do
    sqrt ((a - x)^(2 ::Int) + (b - y)^(2 ::Int) + (c - z)^(2 ::Int))
