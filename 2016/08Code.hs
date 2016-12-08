import Data.List (nub) 

type Screen = [(Int, Int)]

screen :: Screen 
screen = []

rect :: Int -> Int -> Screen  -> Screen
rect x y s =  [(x', y') | x' <- [0..(x-1)]
                        , y' <- [0..(y-1)]
                        ] ++ s


rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn column n xs =
    map (rotateN column n) xs
    where rotateN column n (x, y) =
                if x == column
                    then (x, (y + n) `mod` 6)  
                    else (x, y)

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow row n xs =
    map (rotateN row n) xs
    where rotateN row n (x, y) =
                if y == row 
                    then ((x + n) `mod` 50, y)  
                    else (x, y)


parse :: String -> (Screen -> Screen)
parse str = 
    case words str of
        ["rect", n]                           -> parseRect n
        ["rotate", "column", column, "by", n] -> parseRotate column n rotateColumn 
        ["rotate", "row", row, "by", n]       -> parseRotate row n rotateRow
    where parseRect n =
                let x = read $ takeWhile (/='x') n
                    y = read . tail $ dropWhile (/='x') n
                in  rect x y 
          
          parseRotate column n f =
                let x  = read . tail $ dropWhile (/='=') column
                    n' = read n 
                in f x n' 
         
applyAll :: [Screen -> Screen] -> (Screen -> Screen)
applyAll []     = id 
applyAll (f:xs) = (applyAll xs) . f

countOn :: Screen -> Int 
countOn xs = length $ nub xs
   
main = do
    l <- readFile "08input.txt"
    let parsed = map parse $ lines l
    let f = applyAll parsed
    let result1 = f screen
    printScreen result1
    putStrLn . show $ countOn result1 

-- debug stuff
showLine :: Screen -> Int -> String
showLine xs n =
    let onLights = map fst $ filter ((n ==). snd) xs
    in [ if isOn then '#' else '.' | x <- [0..49]
                                   , let isOn = x `elem` onLights ]
    
showScreen :: Screen -> [String]
showScreen xs = [showLine xs n | n <- [0..5] ]

printScreen :: Screen -> IO ()
printScreen = mapM_ putStrLn . showScreen
