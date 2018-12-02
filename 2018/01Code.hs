parseInt :: String -> Int
parseInt ('-':xs) = - (read xs)
parseInt ('+':xs) = read xs

parse :: IO [Int]
parse = (map parseInt . lines) `fmap` readFile "01Input.txt"

exercise1 = sum `fmap` parse

exercise2 :: IO Int
exercise2 = do
    nums <- parse
    --let nums = [3, 3, 4, -2, -4]
    let nums' = nums ++ nums'
    return $ seenTwice [0] nums' 0
    

seenTwice :: [Int] -> [Int] -> Int -> Int 
seenTwice seen (x:xs) n =
    let result = n + x
    in if result `elem` seen
            then result
            else seenTwice (result : seen) xs result
