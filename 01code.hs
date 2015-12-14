main :: IO ()
main = do
        -- the first part
        file <- readFile "01Input.txt"
        putStrLn . show $ calcFloor file 
        -- second part
        let input = zip [1..] . map calcFloor $ map (flip take file) [1..length file]
        let result = show . fst . head . filter (\x -> (snd x) == -1) $ input 
        putStrLn result

calcFloor :: String -> Int
calcFloor = sum . map toInt

toInt :: Char -> Int
toInt ')' = -1
toInt '(' =  1
toInt _   =  0
