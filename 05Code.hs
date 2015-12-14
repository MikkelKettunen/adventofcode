import Data.List
import Data.Functor

main :: IO () 
main = do wordsInFile <- lines <$> readFile "05Input.txt"
          let niceWords1 = filter id $ map isNice1 wordsInFile
          putStrLn $ "nice words1: " ++ show (length niceWords1)
          let niceWords2 = filter id $ map isNice2 wordsInFile
          putStrLn $ "nice words2: " ++ show (length niceWords2)

vowelsNice1, doubleLetterNice1, notContainsNice1, isNice1 :: String -> Bool

vowelsNice1 = (>2) . length . filter valid  
        where valid l = any (l==) "aeiou"

doubleLetterNice1 = any (>1) . map length . group

-- must not contain the strings "ab", "cd", "pq", "xy"
notContainsNice1 s = not $ any (\x -> x `isInfixOf` s) ["ab", "cd", "pq", "xy"]

isNice1 s = vowelsNice1 s && doubleLetterNice1 s && notContainsNice1 s

pairsNice2, letterRepeatNice2, isNice2 :: String -> Bool

pairsNice2 s
    | length s >= 4 = let letters = take 2 s
                          rest    = drop 2 s
                      in if letters `isInfixOf` rest
                             then True
                             else pairsNice2 (tail s)
    | otherwise     = False

letterRepeatNice2 s 
    | length s >= 3 = if x == z
                          then True
                          else letterRepeatNice2 (tail s)
    | otherwise     = False
        where x:y:z:xs = s

isNice2 s = pairsNice2 s && letterRepeatNice2 s
