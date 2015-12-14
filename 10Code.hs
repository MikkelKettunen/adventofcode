import Data.List

applied :: [String]
applied = "1113122113" : map apply applied

apply :: String -> String
apply = concat . map (\(chr, len) -> (show len) ++ [chr]) . map (\s -> (head s, length s)) . group 

part1 = length $ applied !! 40
part2 = length $ applied !! 50
