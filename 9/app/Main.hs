module Main where

type Point = (Int,Int)

main :: IO ()
main = do
    print . part1 =<< readFile "./example"
    print . part1 =<< readFile "./puzzle"
    putStrLn "Hello, Haskell!"

part1 :: String -> Int
part1 s = maximum . map (`largestRectangle` points) $ points
    where 
        points = map ((\x -> (head x, x !! 1)) . map read . words . map (\x -> if x == ',' then ' ' else x)) . lines $ s

largestRectangle :: Point -> [Point] -> Int
largestRectangle _ [] = -1
largestRectangle p (x:xs) = max (rectArea p x) (largestRectangle p xs)

rectArea :: Point -> Point -> Int
rectArea a b = l * w
    where
        l = (+1) . abs $ max (snd b) (snd a) - min (snd b) (snd a) 
        w = (+1) . abs $ max (fst a) (fst b) - min (fst a) (fst b)
