module Main where

import Data.List
import Data.Maybe
import Data.Function
import Data.Ord

type Vertex = (Float,Float,Float)

main :: IO ()
main = do
    example <- readFile "./example"
    puzzle <- readFile "./puzzle"
    let input = map ((\[x, y, z] -> (x,y,z)) . map read . splitOn ',') $ lines example ::[Vertex]
    let circuitsWithDistances = map (\x -> (x `getDistances` input,[x])) input
    let exampleSolution = (!! 10) . tail . iterate shortCircuit $ circuitsWithDistances
    let puzzleInput = map ((\[x,y,z] -> (x,y,z)) . map read . splitOn ',') $ lines puzzle :: [Vertex]
    let puzzleSolution = (!! 1001) . tail . iterate shortCircuit . map (\x -> (x `getDistances` puzzleInput,[x])) $ puzzleInput
    -- mapM_ (print . fst) circuitsWithDistances
    print . product . take 3 . sortBy (comparing Data.Ord.Down) . map (length . snd) $ exampleSolution
    print . product . take 3 . sortBy (comparing Data.Ord.Down) . map (length . snd) $ puzzleSolution
    -- print pairedInput
    putStrLn "Placeholder"

shortCircuit :: [([(Float,Vertex)],[Vertex])] -> [([(Float,Vertex)],[Vertex])]
shortCircuit [] = []
shortCircuit s = do
    let sorted = sortOn (minimumBy (compare `on` fst) . fst) s
    let (distances, vertices) = head sorted
    let (_,target) = head distances
    if target `elem` vertices then
        (tail distances, vertices) : tail sorted
    else do
        let targetCircuit = filter (elem target . snd) sorted
        let remainingDistances = sortOn fst (tail distances ++ (tail . fst . head $ targetCircuit))
        let newVertices = vertices ++ (snd . head $ targetCircuit)
        (remainingDistances, newVertices) : (filter (notElem target . snd) .  tail $ sorted)


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = if c `elem` s then takeWhile (/=c) s : (splitOn c . tail . dropWhile (/=c) $ s) else [s]

getDistances :: Vertex -> [Vertex] -> [(Float,Vertex)]
getDistances _ [] = []
getDistances v s = tail . sort . map (\x -> (distance v x, x)) $ s

distance :: Vertex -> Vertex -> Float
distance (a,b,c) (x,y,z) = do
    sqrt ((a - x)^(2 ::Int) + (b - y)^(2 ::Int) + (c - z)^(2 ::Int))
