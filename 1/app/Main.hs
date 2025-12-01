module Main where

main :: IO ()
main = do
 puzzle <- readFile "./puzzle_input"
 example <- readFile "./example_input"
 let instructions_puzzle = lines puzzle
 let instructions_example = lines example
 putStrLn $ "example: total ending zeroes: " ++ show (doInstructionsPart1 instructions_example 50)
 putStrLn $ "example: total passing zeroes: " ++ show (doInstructionsPart2 instructions_example 50)
 putStrLn $ "puzzle: total ending zeroes: " ++ show (doInstructionsPart1 instructions_puzzle 50)
 putStrLn $ "puzzle: total passing zeroes: " ++ show (doInstructionsPart2 instructions_puzzle 50)

doInstructionsPart1 :: [String] -> Integer -> Integer
doInstructionsPart1 [] _ = 0
doInstructionsPart1 (x:xs) n = do
 let direction = head x
 let amount = read $ drop 1 x :: Integer
 let result = rotate direction n amount
 if result == 0 then 1 + doInstructionsPart1 xs result else 0 + doInstructionsPart1 xs result

doInstructionsPart2 :: [String] -> Integer -> Integer
doInstructionsPart2 [] _ = 0
doInstructionsPart2 (x:xs) n = do
 let direction = head x
 let amount = read $ drop 1 x :: Integer
 let result = hitZero direction n amount
 result + doInstructionsPart2 xs (rotate direction n $ mod amount 100)

rotate :: Char -> Integer -> Integer -> Integer
rotate c x y
 | c == 'L' = mod (x - y + 100) 100
 | c == 'R' = mod (x + y + 100) 100
 | otherwise = -1

hitZero :: Char -> Integer -> Integer -> Integer
hitZero c x y
 | x == 0 = div y 100 -- starting at 0 is itself NOT an additional 0 count
 | c == 'L' = do if (x - y) <= 0 then 1 + hitZero c x (y - 100) else 0
 | c == 'R' = do if (x + y) > 99 then 1 + hitZero c x (y - 100) else 0
 | otherwise = 0
