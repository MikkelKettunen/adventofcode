import Data.List 
import Control.Arrow
import Data.Functor

checksum1 = uncurry (*) . (sum *** sum) . unzip . map count . lines
count = (ofLength 3 &&& ofLength 2) . map length . group . sort
ofLength n = min 1 . sum . filter (== n)
exercise1 = checksum1 <$> readFile "02Input.txt"

distance :: String -> String -> Int
distance s1 s2 = length . filter (uncurry (/=)) $ zip s1 s2
findShortestDistances :: [String] -> [(String, String)]
findShortestDistances l =
    let l'  = zip l [1..] 
        l'' = [(dst, x, y) | (x, r1) <- l'
                           , (y, r2) <- l'
                           , let dst = distance x y
                           , r1 /= r2]
        shortests = minimum $ map (\(d, _, _) -> d) l''
        l''' = filter (\(d, _, _) -> d == shortests) l''
   in map (\(_, x, y) -> (x,y)) l'''

makeEqual s1 s2 =
    fst . unzip . filter (uncurry (==))$ zip s1 s2
     
exercise2 = (uncurry makeEqual) . head . findShortestDistances. lines <$> readFile "02Input.txt"

