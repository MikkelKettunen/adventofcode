import Data.Functor
import Data.Array.IO
import Data.Foldable

main :: IO ()
main = 
    fileLines >>= \l -> day6PartN l (const 0) (const 1) (1 -) >>
                        day6PartN l (max 0 . (subtract 1)) (+1) (+2) 
    where fileLines = lines <$> readFile "06Input.txt"

day6PartN :: [String]
          -> (Int -> Int) -- off
          -> (Int -> Int) -- on
          -> (Int -> Int) -- toggle
          -> IO ()
day6PartN l off on toggle =
    let parsed = map (parse (off, on, toggle)) l
    in start >>= \world -> updateWorld parsed world >> countLights world >>= printResult  
    where start = newArray ((0,0), (999, 999)) 0 :: IO (IOArray (Int, Int) Int)
          updateWorld parsed world = forM_ parsed (updateArray world)
          countLights world = sum <$> getElems world           
          printResult count = putStrLn $ "lights on: " ++ show count          


updateArray :: IOArray (Int, Int) Int 
            -> ((Int, Int), (Int, Int), Int -> Int)
            -> IO ()
updateArray array ((x1, x2), (y1, y2), f) =
    let values = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
    in forM_ values  (updateElem array f)


updateElem :: IOArray (Int, Int) Int -> (Int -> Int) -> (Int, Int) -> IO ()
updateElem array f p =
    f <$> readArray array p >>= writeArray array p 
 
parse :: ((Int -> Int), (Int -> Int), (Int -> Int)) 
      -> String
      -> ((Int, Int), (Int, Int), (Int -> Int))
parse (off, on, toggle) s =
    case words $ removeComma s of
        ["turn", "off", x1, y1, "through", x2, y2] -> 
            toOutput x1 x2 y1 y2 off
        ["turn", "on" , x1, y1, "through", x2, y2] -> 
            toOutput x1 x2 y1 y2 on
        ["toggle",      x1, y1, "through", x2, y2] -> 
            toOutput x1 x2 y1 y2 toggle
        where removeComma :: String -> String
              removeComma = map (\x-> if x == ',' then ' ' else x)
              i :: String -> Int
              i = read
              toOutput x1 x2 y1 y2 f = ((i x1, i x2), (i y1, i y2), f)

