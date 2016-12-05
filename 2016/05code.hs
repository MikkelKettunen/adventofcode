import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Prelude hiding (toInteger)

keyword = "reyedfim"

hash :: String -> String 
hash f = show . md5 . C.pack $ f

startsWith5Zeroes :: String -> Bool
startsWith5Zeroes ('0':'0':'0':'0':'0':_) = True
startsWith5Zeroes _                       = False

dropZeroes :: String -> String
dropZeroes = dropWhile (=='0')

validCombinations :: [String]
validCombinations = filter startsWith5Zeroes . map hash . map (keyword ++) $ map show [1..]

passwordHashes :: [String]
passwordHashes = take 8 validCombinations

password :: String
password = map head $ map dropZeroes passwordHashes

toInteger :: Char -> Int
toInteger x = read ('0':'x':x:[]) :: Int

isValidChar :: String -> Bool
isValidChar (x:_) = toInteger x <= 7

validHashes :: [String]
validHashes = filter isValidChar $ map (drop 5) validCombinations

possibleHashes :: [(Int, Char)]
possibleHashes = map f validHashes
    where f l@(x:_) = (toInteger x, head $ drop 1 l)

find' :: Int -> [(Int, Char)] -> (Int, Char)
find' n = head . dropWhile ((n /=) . fst) 
        
password2 :: String
password2 =
    let find'' = flip find'
    in map snd $ map (find'' possibleHashes) [0..7]

main :: IO ()
main = putStrLn password >> putStrLn password2 
