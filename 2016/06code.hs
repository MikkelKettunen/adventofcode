import Data.Map hiding (map)
import Data.Tuple
import Data.List (sort)

countChars :: String -> [(Char, Int)]
countChars input = toList $ fromListWith (+) [(c, 1) | c <- input]

maxUsed :: [(Char, Int)] -> Char
maxUsed = snd . last . sort . map swap 

leastUsed :: [(Char, Int)] -> Char
leastUsed = snd . head . sort . map swap 

-- ["aab", "ccd", "dde", ,"ffg"] -> ["acdf", "acdf", "bdeg"]
splitIntoCharacters :: [String] -> [String]
splitIntoCharacters str =
    let len      = length . head $ str 
        splitter = [0..len-1]
        takes    = map (\n -> map (head . drop n)) splitter
        taken    = map (\f -> f str) takes
    in taken

main = do
    f <- readFile "06Input.txt"
    let result1 = map (maxUsed . countChars) . splitIntoCharacters $ lines f
    putStrLn result1
    let result2 = map (leastUsed . countChars) . splitIntoCharacters $ lines f
    putStrLn result2
