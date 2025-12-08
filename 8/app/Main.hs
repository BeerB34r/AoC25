module Main where

import Data.List

type Vertex = (Float,Float,Float)

main :: IO ()
main = do
    example <- readFile "./example"
    let input = map ((\[x, y, z] -> (x,y,z)) . map read . splitOn ',') $ lines example ::[Vertex]
    let pairedInput = map (take 2) . permutations $ input
    let distancedInput = map (\[x,y] -> distance x y) pairedInput
    print input
    -- print pairedInput
    print distancedInput
    putStrLn "Placeholder"

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = if c `elem` s then takeWhile (/=c) s : (splitOn c . tail . dropWhile (/=c) $ s) else [s]

distance :: Vertex -> Vertex -> Float
distance (a,b,c) (x,y,z) = do
    sqrt ((a - x)^2 + (b - y)^2 + (c - z)^2)
