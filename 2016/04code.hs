import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Tuple as Tpl
import Data.List (sortBy, isInfixOf)
import qualified Data.Ord as O

data Room = Room { rawName       :: [String]
                 , name          :: String 
                 , sector        :: Int
                 , checksum      :: String
                 , decryptetName :: String
                 } deriving (Show)

split :: Char -> String -> [String]
split ch = map T.unpack . T.split (ch==) . T.pack 

countChar :: String -> [(Int, Char)]
countChar str = sortBy sort . map Tpl.swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- str]
    where sort (n1, ch1) (n2, ch2)
            | n1 > n2  = O.LT
            | n1 < n2  = O.GT
            | ch1 < ch2 = O.LT
            | ch1 > ch2 = O.GT
            | otherwise = O.EQ 

toChecksum :: String -> String
toChecksum = take 5 . map snd . countChar

parseRoom :: String -> Room
parseRoom str = 
    let all      = split '-' str
        rawName  = init all
        name     = concat rawName
        sector   = read . head . split '[' $ last all
        checksum = takeWhile (/= ']') . last . split '[' $ last all
    in Room (init all) name sector checksum ""
        
isChecksumValid :: Room -> Bool
isChecksumValid room = checksum room == toChecksum (name room)

decryptName :: Room -> Room 
decryptName room = room {decryptetName = unwords . map (map (decryptChar (sector room))) $ rawName room }
   

decryptChar :: Int -> Char -> Char
decryptChar n ch = foldl apply ch calls
    where calls = map (const decrypt) [1..n]
          
          decrypt 'z' = 'a'
          decrypt ch  = succ ch
          
          apply ch f = f ch

main :: IO ()
main = do file <- readFile "04input.txt"
          let rooms      = map parseRoom $ lines file
          let validRooms = filter isChecksumValid rooms
          let sumSector  = sum $ map sector validRooms
          putStrLn . show $ sumSector
          -- part two
          let decryptetRooms = map decryptName validRooms
          let part2Room = filter (\r -> "northpole" `isInfixOf` (decryptetName r)) decryptetRooms
          mapM_ (putStrLn . show) part2Room

