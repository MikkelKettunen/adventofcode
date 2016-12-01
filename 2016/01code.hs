import Data.Char 

data Vector = Vector Int Int deriving (Show, Eq)

Vector a b |*| scalar = Vector (a * scalar) (b * scalar) 
Vector a b |+| Vector c d = Vector (a + c) (b + d)
Vector a b |-| Vector c d = Vector (a - c) (b - d)

norm (Vector a b) =
    let a' = if a < 0 then -1 else if a > 0 then 1 else 0    
        b' = if b < 0 then -1 else if b > 0 then 1 else 0
    in Vector a' b'

dist (Vector a b) = abs a + abs b

main :: IO ()
main = do
    steps <- map parse <$> words <$> readFile "01input.txt"
    
    let startPosition = Vector 0 0
    let startFacing   = Vector 0 1

    -- first problem
    let (endPos, endDir) = foldl step (startPosition, startFacing) steps 
    putStrLn . show $ dist endPos 


    let endPos2    = step2 [startPosition] (startPosition, startFacing) steps
    let duplicate  = dup 1 endPos2

    putStrLn . show . dist $ duplicate 

    return ()

dup :: Int -> [Vector] -> Vector
dup i l = 
    let taken   = take i l
        element = l !! i
    in if element `elem` taken
            then element
            else dup (succ i) l

step2 :: [Vector] -> (Vector, Vector) -> [(Char, Int)] -> [Vector] 
step2 visited _ [] = reverse $ visited
step2 visited (pos, facing) (x:xs) =
    let (pos', facing') = step' (pos, facing) x
    in step2 (pos' ++ visited) (last pos', facing') xs

from :: Vector -> Vector -> [Vector]
from start end =
    let dir    = end |-| start 
        step   = norm dir
        start' = start |+| step
    in if dist step == 0
            then []
            else start' : from start' end
        

step' :: (Vector, Vector) -> (Char, Int) -> ([Vector], Vector)
step' (pos, facing) (dir, dist) = 
    let facing'  = newFacing dir facing 
        moveDist = facing' |*| dist 
        pos'     = moveDist |+| pos
    in (from pos pos', facing')

step :: (Vector, Vector) -> (Char, Int) -> (Vector, Vector)
step (pos, facing) (dir, dist) = 
    let facing'  = newFacing dir facing 
        moveDist = facing' |*| dist 
        pos'     = moveDist |+| pos
    in (pos', facing')

newFacing :: Char -> Vector -> Vector
newFacing 'R' (Vector 0 1)    = Vector 1 0
newFacing 'R' (Vector 1 0)    = Vector 0 (-1)
newFacing 'R' (Vector 0 (-1)) = Vector (-1) 0
newFacing 'R' (Vector (-1) 0) = Vector 0 1
newFacing 'L' (Vector 0 1)    = Vector (-1) 0
newFacing 'L' (Vector (-1) 0) = Vector 0 (-1)
newFacing 'L' (Vector 0 (-1)) = Vector 1 0
newFacing 'L' (Vector 1 0)    = Vector 0 1


parse :: String -> (Char, Int)
parse (direction:xs) =
    let num = takeWhile isNumber xs
    in (direction, read num)
