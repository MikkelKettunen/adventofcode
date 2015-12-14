import Data.Functor
import Data.Bits
import Data.Char
import Data.Maybe

data Parse = SET String String
           | NOT String String
           | AND String String String
           | OR  String String String
           | LSHIFT String String String
           | RSHIFT String String String
           deriving Show


main :: IO ()
main = do
    fileLines <- lines <$> readFile "07Input.txt" 
    let parsed = map parseLine fileLines
    let result = map (buildResults result parsed) [0..701]
    let aValue = (result !! (stringToInt "a")) :: Int 
    putStrLn $ "part 1:" ++ show aValue 

    -- part 2
    let parsed' = filter (("b" /=) . rValueAsString) parsed
    let parsed'' = (SET (show aValue) "b") : parsed'
    let result' = map (buildResults result' parsed'') [0..701] 
    let aValue' = (result' !! (stringToInt "a")) 
    putStrLn $ "part 2:" ++ show aValue' 

    where buildResults :: [Int] -> [Parse] -> Int -> Int
          buildResults results parsed v = 
              let result = getParseWithValue parsed v   
                  val    = getValueFromParse results <$> result 
              in if isJust val
                     then fromJust val
                     else -1

          getValueFromParse :: [Int] -> Parse -> Int
          getValueFromParse results (SET l _)        = getValue results l 
          getValueFromParse results (NOT l _)        = 65535 - getValue results l
          getValueFromParse results (AND l1 l2 _)    = (getValue results l1) .&. (getValue results l2)
          getValueFromParse results (OR l1 l2 _)     = (getValue results l1) .|. (getValue results l2)
          getValueFromParse results (LSHIFT l1 l2 _) = (getValue results l1) `shiftL` (getValue results l2)
          getValueFromParse results (RSHIFT l1 l2 _) = (getValue results l1) `shiftR` (getValue results l2)
         
          getValue :: [Int] -> String -> Int 
          getValue r l  
              | isNum l   = read l
              | otherwise = r !! (stringToInt l)
		

          getParseWithValue :: [Parse] -> Int -> Maybe Parse
          getParseWithValue parsed v =
               let result = filter (rValue v) parsed
               in if null result
                      then Nothing
                      else Just $ head result

          rValueAsString :: Parse -> String
          rValueAsString (SET _ r)      = r
          rValueAsString (NOT _ r)      = r
          rValueAsString (AND _ _ r)    = r
          rValueAsString (OR _ _ r)     = r
          rValueAsString (LSHIFT _ _ r) = r
          rValueAsString (RSHIFT _ _ r) = r 
          
          rValue :: Int -> Parse -> Bool
          rValue val parse = isLookingFor val $ rValueAsString parse

          isLookingFor :: Int -> String -> Bool
          isLookingFor val r = notNumber r && stringToInt r == val     
 
          notNumber :: String -> Bool
          notNumber = not . isNum  
       

isNum :: String -> Bool
isNum = isNumber . head



parseLine :: String -> Parse
parseLine s =
    case words s of 
        [l, "->", r]                -> SET l r
        ["NOT", l, "->", r]         -> NOT l r
        [l1, "AND", l2, "->", r]    -> AND l1 l2 r
        [l1, "OR", l2, "->", r]     -> OR l1 l2 r
        [l1, "LSHIFT", l2, "->", r] -> LSHIFT l1 l2 r
        [l1, "RSHIFT", l2, "->", r] -> RSHIFT l1 l2 r

charToInt :: Char -> Int
charToInt a = ord a - ord 'a'

stringToInt :: String -> Int
stringToInt (a:[])   = charToInt a
stringToInt (b:a:[]) = charToInt a + (charToInt b + 1) * 26

