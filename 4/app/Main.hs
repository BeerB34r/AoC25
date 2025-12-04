{-# LANGUAGE TupleSections #-}
module Main where


main :: IO ()
main = do
    print . part1 =<< readFile "./example"
    print . part2 =<< readFile "./example"
    print . part1 =<< readFile "./puzzle"
    print . part2 =<< readFile "./puzzle"

part1 :: String -> Int
part1 = countAvailable . drawMap

part2 :: String -> Int
part2 = liftForks . drawMap

liftForks :: [((Int, Int), Char)] -> Int
liftForks x
    | countAvailable x > 0 = countAvailable x + (liftForks . map (\y -> if isAvailable y x then (fst y, '.') else y) $ x)
    | otherwise = 0

isAvailable :: ((Int,Int), Char) -> [((Int,Int),Char)] -> Bool
isAvailable x = (<5) . length . filter (=='@') . getNeighbours x

drawMap :: String -> [((Int,Int),Char)]
drawMap x = concatMap (zipWith (\a (b,c) -> ((a,b),c)) [0..]) . zipWith (\a b -> map (a,) b) [0..] $ [replicate ((length . head . lines $ x) + 2) '.'] ++ map (\y -> "." ++ y ++ ".") (lines x) ++ [replicate ((length . head . lines $ x) + 2) '.']

countAvailable :: [((Int,Int),Char)] -> Int
countAvailable s = length . filter (<5) . map (length . filter (=='@') . (`getNeighbours` s)) . filter (\(_,a) -> a == '@') $ s

getNeighbours ::((Int,Int), Char) -> [((Int,Int),Char)] -> [Char]
getNeighbours x = map snd . filter (\((a,b),_) -> xpos x - 1 <= a && a <= xpos x + 1 && ypos x - 1 <= b && b <= ypos x + 1)
    where
        xpos = fst . fst
        ypos = snd . fst
