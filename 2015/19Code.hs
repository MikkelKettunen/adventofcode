import Data.Functor
import Data.Maybe
import Data.Tuple (swap)
import Data.List (nub, concat, isPrefixOf)
import Control.Arrow

parse :: String -> Maybe (String, String)
parse s =
    case words s of
        [from, "=>", to] -> Just (from, to)
        _                -> Nothing

main :: IO ()
main = lines <$> readFile "19Input.txt" >>= \lines ->
          let replacements    = map fromJust . filter isJust $ map parse lines
              molecule        = last lines
          in part1 molecule replacements 

part1 :: String -> [(String, String)] -> IO () 
part1 molecule replacements =
     let moleculePrefixs = map (flip splitAt molecule) [0..(length molecule -1)]
         replaced        = map (getReplacements replacements) $ moleculePrefixs
         count           = length . nub . filter (/="") . concat $ replaced
     in putStrLn $ "1: " ++ show count 
 
getReplacements :: [(String, String)] -> (String, String) -> [String]
getReplacements replacements str =
    nub . filter (not.null) $ map (count str) replacements
        where count (str1,str2) (from, to) =
                  if from `isPrefixOf` str2
                      then str1 ++ (replace from to str2)
                      else ""

replace :: String -> String -> String -> String
replace from to str =
    let str' = drop (length from) str
    in to ++ str'

