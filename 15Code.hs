import Data.List

data Ingredient = Ingredient { name       :: String,
                               capacity   :: Int,
                               durability :: Int,
                               flavor     :: Int,
                               texture    :: Int,
                               calories   :: Int } deriving (Show)

scoreOverAll :: [(Ingredient -> Int)]-> [Ingredient] -> [Int] -> Int
scoreOverAll mappings ingredients multiplier =
    product $ map (\f -> score f ingredients multiplier) mappings

score :: (Ingredient -> Int) -> [Ingredient] -> [Int] -> Int
score f ingredients multiplier =
    let values = map f ingredients
    in (max 0) . sum . map (uncurry (*)) $ zip values multiplier

parse :: String -> Ingredient
parse s =
    case words s of 
        [name, _, capacity, _, durability, _, flavor, _, texture, _, calories] ->
            Ingredient name (toInt capacity) (toInt durability) (toInt flavor) (toInt texture) (toInt calories)
    where toInt :: String -> Int
          toInt s = if last s == ','
                        then read $ init s
                        else read s


input = [parse "Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5",
         parse "PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1",
         parse "Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6",
         parse "Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8"]

possibleCombinations = [[a,b,c,d] | a <- [0..100]
                                  , b <- [0..(100-a)]
                                  , c <- [0..(100-b)]
                                  , let d = 100 - (a+b+c)
                                  , a+b+c+d == 100
                                  , d >= 0]

part1 = maximum $ map (scoreOverAll [capacity, durability, flavor, texture] input) possibleCombinations

possibleCombinations' = filter ((500==) . (score calories input)) possibleCombinations 

part2 = maximum $ map (scoreOverAll [capacity, durability, flavor, texture] input) possibleCombinations'
