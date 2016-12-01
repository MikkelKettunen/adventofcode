import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as C
main :: IO()
main = do
        let myKey = "ckczppom"
        let myKeys = map (\x -> myKey ++ (show x)) [1..]
        let hashed = zip [1..] $ map hash myKeys
        let result = 
              fst . head . filter (startsWith5Zeroes . snd ) $ hashed
        putStrLn $ "5 zeroes: " ++  (show result)
        let result' = 
              fst . head . filter (startsWith6Zeroes . snd ) $ hashed
        putStrLn $ "6 zeroes: " ++  (show result')

-- slow method of testing if they start with n zeroes
startsWith5Zeroes, startsWith6Zeroes :: String -> Bool
startsWith5Zeroes ('0':'0':'0':'0':'0':_) = True
startsWith5Zeroes _ = False
startsWith6Zeroes ('0':'0':'0':'0':'0':'0':_) = True
startsWith6Zeroes _ = False

-- slow hash function
hash :: String -> String 
hash f = show . md5 . C.pack $ f
