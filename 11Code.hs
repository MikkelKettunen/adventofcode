import Data.List (isInfixOf, group)

threeIncreasingLetters :: String -> Bool
threeIncreasingLetters s =
    let letters = ['a'..'x']
        mustContainOne = [a:b:c:[] | a <- letters
                                   , let b = succ a
                                   , let c = succ b]
    in any (\x -> x `isInfixOf` s) mustContainOne

notContainsIOL :: String -> Bool
notContainsIOL s = not $ any (\x -> x `isInfixOf` s) ["i","o","l"]

minTwoPairs :: String -> Bool
minTwoPairs = (4 <=) . sum . filter (>1) . map length . group 

isValidPassword :: String -> Bool
isValidPassword s =
    let increasingLetters = threeIncreasingLetters s
        notIOL = notContainsIOL s
        pairs = minTwoPairs s
    in increasingLetters && notIOL && pairs


nextLetter :: Char -> Char
nextLetter 'z' = 'a'
nextLetter c   = succ c

nextPassword :: String -> String
nextPassword s =
    let updateIf True char  = nextLetter char
        updateIf False char = char
        
        updatePassword 'z' (string, update) = (updateIf update 'z' : string, update)
        updatePassword chr (string, update) = (updateIf update chr : string, False)
   in fst $ foldr updatePassword ("", True) s


findNextPassword :: String -> String
findNextPassword s =
    let list = s : map nextPassword list
    in head $ filter isValidPassword list


main = let password1 = findNextPassword "hepxcrrq"
           password2 = findNextPassword (nextPassword password1)
       in putStrLn ("password: " ++ password1) >> 
          putStrLn ("password2: " ++ password2)
