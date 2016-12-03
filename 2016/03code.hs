import Data.List (sort)
import Control.Arrow

main :: IO ()
main = readFile "03Input.txt" >>= putStrLn . show . (problem1 &&& problem2)

isValidTriangle :: [Int] -> Bool
isValidTriangle [a, b, c] = a + b > c

problem2 :: String -> Int
problem2 = length . filter isValidTriangle . map sort . parse2 . map (map read) . map words . lines

parse2 :: [[Int]] -> [[Int]]
parse2 list = 
    let x        = take 3 list
        xs       = drop 3 list 
        selector = [[(b,a)|b <- [0..2]] | a <- [0..2]] :: [[(Int, Int)]]
        selected = map select selector
        select   = map (\(a,b) -> ((x !! a) !! b)) 
    in if null xs
            then selected
            else selected ++ parse2 xs

problem1 :: String -> Int
problem1 = length . filter isValidTriangle . map parse . lines 

parse :: String -> [Int]
parse = sort . map read . words
