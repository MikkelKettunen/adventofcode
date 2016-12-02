data Vector = Vector Int Int deriving Show

(|+|) :: Vector -> Vector -> Vector
Vector a b |+| Vector c d = Vector (a + c) (b + d)

dist :: Vector -> Int
dist (Vector a b) = abs a + abs b

clamp1 :: Vector -> Vector -> Vector
clamp1 _ (Vector a b) = 
    let a' = min 2 . max 0 $ a
        b' = min 2 . max 0 $ b
    in Vector a' b'

clamp2 :: Vector -> Vector -> Vector
clamp2 pos pos' =
    if dist pos' > 2
        then pos
        else pos'
    

main :: IO ()
main = do
    line <- lines <$> readFile "02input.txt"
    let positions1 = loop clamp1 (Vector 1 1) line
    let code1      = map toCode1 positions1
    putStrLn . concat $ code1

    let positions2 = loop clamp2 (Vector (-2) 0) line
    let code2      = map toCode2 positions2
    putStrLn . concat $ code2
 
    return ()

toCode1 :: Vector -> String 
toCode1 (Vector a b) = show $ a + 3*b + 1

toCode2 :: Vector -> String 
toCode2 (Vector a b) =
    case (a, b) of
        ( 0, -2) -> "1"
        (-1, -1) -> "2"
        ( 0, -1) -> "3"
        ( 1, -1) -> "3"
        (-2,  0) -> "5"
        (-1,  0) -> "6"
        ( 0,  0) -> "7"
        ( 1,  0) -> "8"
        ( 2,  0) -> "9"
        (-1,  1) -> "A"
        ( 0,  1) -> "B"
        ( 1,  1) -> "C"
        ( 0,  2) -> "D"
        


loop :: (Vector -> Vector -> Vector) -> Vector -> [String] -> [Vector]
loop _     _   []     = []
loop clamp pos (x:xs) =
    let pos' = foldl (step clamp) pos x
    in pos' : loop clamp pos' xs
    

step :: (Vector -> Vector -> Vector) -> Vector -> Char -> Vector
step clamp position ch = 
    let position' = position |+| direction ch 
    in clamp position position'

direction 'L' = Vector (-1) 0
direction 'R' = Vector 1 0
direction 'D' = Vector 0 1
direction 'U' = Vector 0 (-1)
