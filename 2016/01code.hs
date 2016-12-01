main :: IO ()
main = do
    words <- map parse <$> words <$> readFile "01input.txt"

    putStrLn . show $ words

    return ()

parse :: String -> (Char, Char)
parse (direction:distance:xs) = (direction, distance)
