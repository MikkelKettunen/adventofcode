import Control.Arrow
import Data.List

main :: IO ()
main = do input <- readFile "03Input.txt"
          let moves = foldr moveAndSave [(0,0)] input
          let presents = calculatePresents moves 
          putStrLn $ "one present: " ++ (show presents)
          -- part two
          let (moving1, moving2) = 
		applyBoth (foldr moveAndSave [(0,0)]) (splitInput input ("",""))
          let presents' =  calculatePresents $ moving1 ++ moving2
          putStrLn $ "two present: " ++ show presents' 

applyBoth :: (a->b) -> (a,a) -> (b,b)
applyBoth f = f *** f

splitInput :: String -> (String, String) -> (String, String)
splitInput (x:y:xs) (a,b) = splitInput xs (x:a, y:b)
splitInput [] r = r

calculatePresents :: [(Int, Int)] -> Int
calculatePresents = length . nub

moveAndSave :: Char -> [(Int, Int)] -> [(Int, Int)]
moveAndSave chr (x:xs) = move chr x : x : xs

move :: Char -> (Int, Int) -> (Int, Int)
move '<' = first (subtract 1)
move '>' = first (+1)
move '^' = second (+1)
move 'v' = second (subtract 1)

