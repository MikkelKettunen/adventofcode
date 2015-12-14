import Data.List (permutations, nub)
import Data.Functor ((<$>))

main = do 
	routes <- concat . map parse . lines <$> readFile "09Input.txt"
	let permutations = namesPermutation routes
	let pricedRoutes = map (calcPrice routes) permutations
	
	putStr "shortest path "  
	putStrLn . show . minimum $ pricedRoutes

	putStr "longest path "
	putStrLn . show . maximum $ pricedRoutes

	where 
	      parse :: String -> [(String, String, Int)]
	      parse line = case words line of [a, "to", b, "=", c] -> [(a,b, (read c) :: Int), (b,a, (read c) :: Int)]
	      
	      namesPermutation :: [(String, String, Int)] -> [[String]]
	      namesPermutation = permutations . nub . concat . map (\(a,b,_) -> [a,b])
	      
	      p :: (String, String, Int) -> Int
	      p (_, _, c) = c
	      
	      price :: [(String, String, Int)] -> (String, String) -> Int	
	      price route (a, b) = p . head . filter (\(a', b', _) -> a == a' && b == b') $ route
 
	      calcPrice :: [(String, String, Int)] -> [String] -> Int
	      calcPrice c r = sum . map (price c) $ zip r (tail r)
