import Data.Functor

data RaindeerState = RaindeerFlying | RaindeerResting deriving (Show, Eq)

data Raindeer = Raindeer { name             :: String,
                           speed            :: Int,
                           speedTime        :: Int,
                           restTime         :: Int,
                           distanceTraveled :: Int,
                           currently        :: RaindeerState,
                           ticksSinceState  :: Int,
                           points           :: Int
                         } deriving Show

main :: IO ()
main = do raindeers <- map parse . lines <$> readFile "14Input.txt"
          let raindeers' = doTicks 2503 raindeers 
          putStrLn . show $ findMax distanceTraveled raindeers'
          putStrLn . show $ findMax points raindeers'

doTicks :: Int -> [Raindeer] -> [Raindeer]
doTicks 0 raindeers = raindeers
doTicks n raindeers = doTicks (n - 1) . updatePoints $ map tick raindeers

findMax :: (Raindeer -> Int) -> [Raindeer] -> Int
findMax f = maximum . map f

updatePoints :: [Raindeer] -> [Raindeer]
updatePoints raindeers =
    let max = findMax distanceTraveled raindeers
        updateWhenMax max raindeer =
            let dist          = distanceTraveled raindeer
                currentPoints = points raindeer
            in if dist == max 
                   then raindeer {points = currentPoints + 1}
                   else raindeer
    in map (updateWhenMax max) raindeers

parse :: String -> Raindeer
parse s =
    case words s of 
        [name, _, _, flyspeed, _, _, speedTime, _, _, _, _, _, _, restTime, _] ->
            Raindeer name (toInt flyspeed) (toInt speedTime) (toInt restTime) 0 RaindeerFlying 0 0
        where toInt :: String -> Int
              toInt = read

tick :: Raindeer -> Raindeer
tick raindeer = 
    let newTick      = 1 + ticksSinceState raindeer
        speedtime    = speedTime raindeer
        resttime     = restTime raindeer
        
        swapState :: RaindeerState -> RaindeerState
        swapState RaindeerFlying  = RaindeerResting
        swapState RaindeerResting = RaindeerFlying
       
        getTimeByState :: RaindeerState -> Raindeer -> Int
        getTimeByState RaindeerFlying  = speedTime
        getTimeByState RaindeerResting = (subtract 2) . restTime

        currentTime = getTimeByState (currently raindeer) raindeer

        clearTicksSinceState :: Raindeer -> Raindeer
        clearTicksSinceState raindeer = raindeer { ticksSinceState = 0 } 

        updateState :: Raindeer -> Raindeer
        updateState raindeer = raindeer { currently = (swapState (currently raindeer)) }
         
        updateDistance :: Raindeer -> Raindeer
        updateDistance raindeer =
            let distance     = distanceTraveled raindeer
                currentSpeed = speed raindeer
                newDistance  = distance + currentSpeed
                state        = currently raindeer
            in if state == RaindeerResting
                   then raindeer
                   else raindeer {distanceTraveled = distance + currentSpeed}  
 
    in if currentTime < newTick
           then clearTicksSinceState $ updateState raindeer
           else updateDistance $ raindeer {ticksSinceState = newTick}


{--
tests :: IO()
tests = do
    let list = [test1, test2]
    forM_ list (putStrLn . (\(a,b) -> (show (distanceTraveled a)) ++ " -> " ++ (show b))) 

dancer :: Raindeer
dancer = parse "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds." 

test1 :: (Raindeer, Int)
test1 = let raindeer = head $ doTicks 5 [dancer]
        in (raindeer, 27*5)

test2 :: (Raindeer, Int)
test2 = let raindeer = head $ doTicks (5+132*2) [dancer]
        in (raindeer, (27*5*2)) --}
