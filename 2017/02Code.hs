import Control.Arrow
import Control.Category hiding (id, (.))
import Data.List

input :: IO [[Int]]
input = map (map read) . map words . lines <$> readFile "02Data.txt"

checksum1 = sum . map (uncurry (-)) . uncurry zip . (map maximum &&& map minimum) <$> input 


findDivisibles :: [Int] -> Int
findDivisibles (x: xs) = 
    let r = filter ((0 ==) . rem x) xs
    in if null r
            then findDivisibles xs
            else x `div` (head r) 

checksum2 = sum . map findDivisibles . map reverse . map sort <$> input 


