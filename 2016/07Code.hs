import Data.Functor

data IPv7 = IPv7 { inside  :: [String]
                 , outside :: [String]
                 } deriving (Show)

fourCharSequence :: String -> Bool
fourCharSequence (a:b:c:d:xs) = 
    if a == d && b == c && a /= b
        then True
        else fourCharSequence (b:c:d:xs) 
fourCharSequence _            = False

threeCharsSequence :: String -> [String]
threeCharsSequence str = 
    [ str' | n <- [0..(length str) - 3]
           , let str' = take 3 $ drop n str
           , let (a:b:c:[]) = str'
           , a == c && a /= b]

flipThreeChars :: String -> String
flipThreeChars (a:b:c) = (b:a:b:[])

createIPv7 :: String -> IPv7
createIPv7 str =
    let (inside, outside) = takeOutside str ([], [])
    in IPv7 inside outside

takeInside :: String -> ([String], [String]) -> ([String], [String])
takeInside []  result            = result
takeInside str (inside, outside) =
    let inside' = takeWhile (/=']') str
        rest    = dropWhile (/=']') str
        result  = (inside':inside, outside)
    in if null rest
            then result
            else takeOutside (tail rest) result

takeOutside :: String -> ([String], [String]) -> ([String], [String])
takeOutside []  result            = result
takeOutside str (inside, outside) =
    let outside' = takeWhile (/='[') str
        rest     = dropWhile (/='[') str
        result   = (inside, outside':outside)
    in if null rest
        then result
        else takeInside (tail rest) result 

supportTLS :: IPv7 -> Bool
supportTLS (IPv7 inside outside) =
    any fourCharSequence outside &&
    not (any fourCharSequence inside)

supportSSL :: IPv7 -> Bool
supportSSL (IPv7 inside outside) = 
    let split'   = concat . filter (not . null) . map threeCharsSequence 
        inside'  = split' inside
        outside' = map flipThreeChars $ split' outside
        result = (not . null) [True | w1 <- inside'
                                    , w2 <- outside'
                                    , w1 == w2]
    in result


main :: IO ()
main = do 
    ipv7 <- map createIPv7 . lines <$> readFile "07input.txt"
    let tls = filter supportTLS ipv7
    putStrLn . show . length $ tls
    let ssl = filter supportSSL ipv7
    putStrLn . show $ length ssl
