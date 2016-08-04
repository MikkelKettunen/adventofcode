import Data.Char
import Data.List

data JSON = JSONObject [(JSON, JSON)]
          | JSONArray [JSON]
          | JSONString String
          | JSONNumber Int
          deriving (Show, Eq)


main :: IO () 
main = do file <- readFile "12Input.txt"
          let (json, _) = parse file
          putStrLn . show . part1 $ json
          putStrLn . show . part2 $ json

part1 :: JSON -> Int
part1 = sumJSON

part2 :: JSON -> Int
part2 = sumJSON . removeRed

removeRed :: JSON -> JSON
removeRed (JSONObject l) =
    if (JSONString "red") `elem` (map snd l)
        then (JSONObject [])
        else (JSONObject (map (\(a,b) -> (a, removeRed b)) l))  
removeRed (JSONArray l)  = (JSONArray (map removeRed l))
removeRed x              = x


sumJSON :: JSON -> Int 
sumJSON (JSONObject l) = sumJSONList (map fst l) + sumJSONList (map snd l)
sumJSON (JSONArray l ) = sumJSONList l
sumJSON (JSONNumber n) = n
sumJSON _              = 0

sumJSONList :: [JSON] -> Int
sumJSONList = foldr ((+) . part1) 0
 
parseJSONNumber :: String -> (JSON, String)
parseJSONNumber xs@(x:rest) = 
    if x == '-'
        then let (JSONNumber n, str) = parseNumber rest 0
             in (JSONNumber (-n), str)
        else parseNumber xs 0
    where 
          parseNumber []       num = (JSONNumber num, [])
          parseNumber l@(n:xs) num = 
              if isNumber n 
                  then parseNumber xs (num * 10 + digitToInt n)
                  else (JSONNumber num, l)

parseJSONString :: String -> (JSON, String)
parseJSONString ('\"':xs) =
    let (str, rest) = addLetter xs ""
    in (JSONString str, rest)
    where addLetter ('\"':xs) str = (reverse str, xs)
          addLetter (x:xs)    str = addLetter xs (x:str)


parseJSONArray :: String -> (JSON, String)
parseJSONArray ('[':']':xs) = (JSONArray [], xs) 
parseJSONArray ('[':xs)     = internalParse xs (JSONArray [])
    where 
          internalParse :: String -> JSON -> (JSON, String)
          internalParse (']':xs) json          = (json, xs)
          internalParse xs       (JSONArray l) = 
              let (json, xs') = parse xs
              in internalParse xs' (JSONArray (l ++ [json]))  

parseJSONObject :: String -> (JSON, String)
parseJSONObject ('{':'}':xs) = (JSONObject [], xs)
parseJSONObject ('{':xs)     = internalParse xs (JSONObject [])
    where
          internalParse :: String -> JSON -> (JSON, String)
          internalParse ('}':xs) json           = (json, xs)
          internalParse xs       (JSONObject l) = 
              let (key, (':':xs'))  = parse xs
                  (val, xs'')       = parse xs'  
              in internalParse xs'' (JSONObject (l ++ [(key, val)]))

parse :: String -> (JSON, String)
parse []     = (JSONArray [], [])
parse xs@(x:xs') =
    case x of
        '\"' -> parseJSONString xs
        '['  -> parseJSONArray xs
        '{'  -> parseJSONObject xs
        ','  -> parse xs' 
        _    -> parseJSONNumber xs
    
    
