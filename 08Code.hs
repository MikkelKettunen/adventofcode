import Data.Functor

main :: IO ()
main = do
    fileLines <- lines <$> readFile "08Input.txt"
    let stringCode = sum $ map countCharactersOfCode fileLines 
    let characters = sum $ map countCharactersOfData fileLines 

    putStrLn $ "part1: " ++ (show (stringCode - characters))

    let encodedLength = sum $ map length $ map show fileLines
    putStrLn $ "part2: " ++ (show (encodedLength - stringCode))


countCharactersOfCode :: String -> Int 
countCharactersOfCode = length 

decode :: String -> String 
decode ('\\':'x':_:_:xs) = ('a': decode xs)
decode ('\\':'\\':xs)    = ('\\':decode xs)
decode ('\\':a:xs)       = ('a':decode xs)
decode (v:xs)            = (v:decode xs)
decode []                = []

countCharactersOfData :: String -> Int
countCharactersOfData = (subtract 2) . length . decode  

