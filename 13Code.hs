import Data.Functor
import Data.List

import Debug.Trace as Debug

main :: IO () 
main = do
    parsed <- map parseLine. lines <$> readFile "13Input.txt"
    putStrLn . show $ part1 parsed
    putStrLn . show $ part2 parsed

part1 :: [(String, Int, String)] -> Int
part1 parsed = 
    let users     = nub $ map (\(n, _, _) -> n) parsed
        usersList = filter (("Alice"==) . head) $ permutations users 
    in maximum $ map (valueOfList parsed) usersList 

part2 :: [(String, Int, String)] -> Int
part2 parsed =
    let users :: [String]
        users   = nub $ map (\(n, _, _) -> n) parsed
        
        parsed' :: [(String, Int, String)]
        parsed' = parsed ++ [(u, 0, "me") | u <- users]
 
        users'  = users ++ ["me"]
        usersList = filter (("me"==) . head) $ permutations users'
    in maximum $ map (valueOfList parsed') usersList

isListIncreasing :: [String] -> Bool
isListIncreasing []       = True
isListIncreasing [x]      = True
isListIncreasing (x:y:xs) = x < y && isListIncreasing xs

valueOfList :: [(String, Int, String)] -> [String] -> Int
valueOfList happiness list =
    let list'  = list ++ [head list]
        list'' = zip (init list') (tail list')
        happi  = map (happinessChange happiness) list''
        result = sum happi
    in result

happinessChange :: [(String, Int, String)] -> (String, String) -> Int
happinessChange list (n1, n2) =
    let getWithName name1 name2 = sum
                                . map (\(_,v,_) -> v) 
                                $ filter (\(n1, _, n2) -> name1 == n1 && name2 == n2) list 
    in getWithName n1 n2 + getWithName n2 n1

parseLine :: String -> (String, Int, String)
parseLine str =
    case words str of
        [name1, _, "lose" , happiness, _, _, _, _, _, _ ,name2] -> 
             (name1, -(read happiness :: Int), init name2) 
        [name1, _, "gain" , happiness, _, _, _, _, _, _, name2] -> 
             (name1, (read happiness :: Int), init name2)
