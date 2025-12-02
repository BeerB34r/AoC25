module Main where

main :: IO ()
main = do
 example_file <- readFile "./example"
 puzzle_file <- readFile "./puzzle"
 print $ part1 example_file
 print $ part1 puzzle_file
 print $ part2 example_file
 print $ part2 puzzle_file

-- Porcelain

part1 :: String -> Int
part1 = let
 isInvalid :: String -> Bool
 isInvalid s
  | odd (length s) = False
  | even (length s) = uncurry (==) . splitAt (length s `div` 2) $ s
  | otherwise = False
 in
  sum
  .concatMap (map read . filter isInvalid . map show . makeRange . words)
  .lines
  .map (\x -> if x == ',' then '\n' else if x == '-' then ' ' else x)


part2 :: String -> Int
part2 = let
  isInvalid :: String -> Bool
  isInvalid n = n `elem` map (\x -> take (length n) . cycle . take x $ n) (factors . length $ n)
 in
  sum
  .concatMap (map read . filter isInvalid . map show . makeRange . words)
  .lines
  .map (\x -> if x == ',' then '\n' else if x == '-' then ' ' else x)

-- Helpers
makeRange :: [String] -> [Int]
makeRange [x,y] = [(read x ::Int)..(read y :: Int)]
makeRange _ = []

factors:: Integral a => a -> [a]
factors n = let
  createList :: Integral a => a -> a -> [a]
  createList x f
   | f <= x `div` 2 = if isFactorOf x f then f : next else next
   | otherwise = []
   where next = createList x (f + 1)
  isFactorOf :: Integral a => a -> a -> Bool
  isFactorOf x y = x `mod` y == 0
 in do createList n 1
