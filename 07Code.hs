import Data.Functor
import Data.Bits
import Data.Char


data Atom = VAL Int | VAR String  deriving Show

data Circuit = SET Atom
             | NOT Atom 
             | AND Atom Atom 
             | OR Atom Atom
             | LSHIFT Atom Atom
             | RSHIFT Atom Atom 
             deriving Show 

main :: IO ()
main = 
    do fileLines <- lines <$> readFile "07Input.txt"
       let parsed = map parse fileLines 
       putStrLn "test"

getDoneCircuit :: [(Circuit, String)] -> [(Int, String)]
getDoneCircuit circuit =
    let completeAtom  = filter (isAtomVal . fst) circuit
    in map getAtomVal completeAtom 
    where -- find all atoms that are just (VAL a) 
          isAtomVal :: Circuit -> Bool
          isAtomVal (SET (VAL v)) = True
          isAtomVal _             = False 
          -- extract value from it 
          getAtomVal :: (Circuit, String) -> (Int, String)
          getAtomVal ((SET (VAL v)), s) = (v, s) 

updateCircuit :: [(Circuit, String)] -> [(Int, String)] -> [(Circuit, String)]
updateCircuit current updates =
    -- search tree for places where  
    map (updateCircuit updates) current
    where updateCircuit :: [(Int, String)] -> (Circuit, String) -> (Circuit, String)
          updateCircuit updates current = current 
         
 
          containsVar :: String -> Circuit -> Bool
          containsVar str c
              let atoms = getAtomFromCircuit c
                  names = map atomName atoms 
              in any (str==) names 

getAtomFromCircuit :: Circuit -> [Atom]
getAtomFromCircuit (SET a)        = [a]
getAtomFromCircuit (NOT a)        = [a]
getAtomFromCircuit (AND a1 a2)    = [a1, a2] 
getAtomFromCircuit (OR a1 a2)     = [a1, a2]
getAtomFromCircuit (LSHIFT a1 a2) = [a1, a2]
getAtomFromCircuit (RSHIFT a1 a2) = [a1, a2]


atomName :: Atom -> String
atomName (VAR s) = s

deleteDoneCircuit :: [(Circuit, String)] -> [(Int, String)] -> [(Circuit, String)]
deleteDoneCircuit cur done =
   let toBeRemoved = map snd done
       contains str = not . null $ filter (str==) toBeRemoved
   in filter (not . contains . snd) cur 
  

parse :: String -> (Circuit, String)
parse s =
    case words s of 
        [l, "->", r]                -> (SET (createAtom l), r)
        ["NOT", l, "->", r]         -> (NOT (createAtom l), r)
        [l1, "AND", l2, "->", r]    -> (AND (createAtom l1) (createAtom l2), r)
        [l1, "OR", l2, "->", r]     -> (OR (createAtom l1) (createAtom l2), r) 
        [l1, "LSHIFT", l2, "->", r] -> (LSHIFT (createAtom l1) (createAtom l2), r)
        [l1, "RSHIFT", l2, "->", r] -> (RSHIFT (createAtom l1) (createAtom l2), r)

createAtom :: String -> Atom
createAtom s = 
    if isNumber . head $ s
        then VAL (read s)
        else VAR s
