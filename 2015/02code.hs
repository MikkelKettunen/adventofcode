import Control.Arrow ((&&&))
import Data.List (sort)

main :: IO ()
main = do file <- readFile "02Input.txt"
          -- part one
          let parsed = map parse . lines $ file
          let totalWrap = sum . map squareFeet $ parsed
          putStrLn $ "total wrap: " ++ (show totalWrap)
          -- part two
          let totalRibbon = sum . map addResult $ map (ribbonBow &&& ribbonAround) parsed 
          putStrLn $ "total ribbon: " ++ show totalRibbon

addResult :: (Int, Int) -> Int
addResult (a,b) = a + b

ribbonBow :: (Int, Int, Int) -> Int
ribbonBow (l,w,h) = l * w * h  

ribbonAround :: (Int, Int, Int) -> Int
ribbonAround (l,w,h) = (*2) . sum . take 2 $ sort [l,w,h]

squareFeet :: (Int, Int, Int) -> Int
squareFeet (l,w,h) = 2 *  (l * w + l * h + w * h) + minimum [l*w, w*h, h*l]

parse :: String -> (Int, Int, Int)
parse s = 
        case words (removeX s) of
             [l, w, h] -> (read l, read w, read h)        
        where removeX = map xToSpace 
              xToSpace 'x' = ' '
              xToSpace  a  =  a

