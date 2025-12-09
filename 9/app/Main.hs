module Main where

type Point = (Int,Int)

main :: IO ()
main = do
    print $ rectArea (2,5) (9,7)
    putStrLn "Hello, Haskell!"

rectArea :: Point -> Point -> Int
rectArea a b = do
    let c = (fst a, snd b)
    let l = abs (max(snd c,snd a) - min(snd c, snd a))
    let w = abs (max(fst c,fst b) - min(fst c, fst b))
    if l == 0 then if w == 0 then 0 else l * w else w
