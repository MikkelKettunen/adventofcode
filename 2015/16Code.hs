import Data.Maybe
import Data.Functor

data Aunt = Aunt { name        :: String,
                   children    :: Maybe Int,
                   cats        :: Maybe Int,
                   samoyeds    :: Maybe Int,
                   pomeranians :: Maybe Int,
                   akitas      :: Maybe Int,
                   vizslas     :: Maybe Int,
                   goldfish    :: Maybe Int,
                   trees       :: Maybe Int,
                   cars        :: Maybe Int,
                   perfumes    :: Maybe Int } deriving Show

createAunt :: String -> Aunt
createAunt name = Aunt name Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

isEqual :: ((Aunt -> Maybe Int), Int, (Int -> Int -> Bool)) -> Aunt -> Bool
isEqual (f, val, test) aunt =
    let check Nothing  = True
        check (Just v) = v `test` val
    in check $ f aunt

parse :: String -> Aunt
parse s = 
    case words s of
        (_:id:xs) -> parseValues xs $ createAunt id
    where parseValues :: [String] -> Aunt -> Aunt
          parseValues []       a = a
          parseValues (n:v:xs) a = 
              let value = Just $ toInt v
                  a' = case n of
                           "children:"    -> a {children    = value}
                           "cats:"        -> a {cats        = value}
                           "samoyeds:"    -> a {samoyeds    = value}
                           "pomeranians:" -> a {pomeranians = value}
                           "akitas:"      -> a {akitas      = value}
                           "vizslas:"     -> a {vizslas     = value}
                           "goldfish:"    -> a {goldfish    = value}
                           "trees:"       -> a {trees       = value} 
                           "cars:"        -> a {cars        = value}
                           "perfumes:"    -> a {perfumes    = value}
              in parseValues xs a' 

          toInt :: String -> Int
          toInt s = if last s == ','
                        then read $ init s
                        else read s 

tickerTape = [(children,    3, (==)),
              (cats,        7, (==)),
              (samoyeds,    2, (==)),
              (pomeranians, 3, (==)),
              (akitas,      0, (==)),
              (vizslas,     0, (==)),
              (goldfish,    5, (==)),
              (trees,       3, (==)),
              (cars,        2, (==)),
              (perfumes,    1, (==))]

equalFunctions = map isEqual tickerTape 

applyEqualFunctions []     aunts = aunts 
applyEqualFunctions (x:xs) aunts = applyEqualFunctions xs $ filter x aunts

input :: IO [Aunt]
input = map parse . lines  <$> readFile "16Input.txt"

part1 = name . head . applyEqualFunctions equalFunctions <$> input

tickerTape2 = [(children,    3, (==)),
               (cats,        7, (>)),
               (samoyeds,    2, (==)),
               (pomeranians, 3, (<)),
               (akitas,      0, (==)),
               (vizslas,     0, (==)),
               (goldfish,    5, (<)),
               (trees,       3, (>)),
               (cars,        2, (==)),
               (perfumes,    1, (==))]

equalFunctions' = map isEqual tickerTape2

part2 = name . head . applyEqualFunctions  equalFunctions' <$> input
