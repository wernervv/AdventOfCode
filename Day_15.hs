{-# LANGUAGE TupleSections #-}

import Data.Bifunctor(second)
import Data.List(sort)

type Coord = (Int, Int)
type Risk = Int
type Element = (Risk,TotalRisk,FromCoord)
type Cavern = [[Element]]
type RiskCoord = (Coord,TotalRisk)
type CheapestCoords = [RiskCoord]

data FromCoord = C Coord | NullCoord
data TotalRisk = R Risk | NullRisk

instance Show FromCoord where
    show NullCoord = "NullCoord"
    show (C coord) = "C " ++ show coord

instance Show TotalRisk where
    show NullRisk = "NullRisk"
    show (R risk) = "R " ++ show risk

instance Eq TotalRisk where
    NullRisk == NullRisk = True
    (R risk1) == (R risk2) = risk1 == risk2
    _ == _ = False

instance Ord TotalRisk where
    compare NullRisk _ = GT
    compare _ NullRisk = LT
    compare (R risk1) (R risk2) = compare risk1 risk2

instance Num TotalRisk where
    NullRisk + NullRisk = NullRisk
    NullRisk + r = r
    r + NullRisk = r
    (R risk1) + (R risk2) = R (risk1 + risk2)
    NullRisk * NullRisk = NullRisk
    NullRisk * r = r
    r * NullRisk = r
    (R risk1) * (R risk2) = R (risk1 * risk2)
    abs NullRisk = NullRisk
    abs (R risk) = R (abs risk)
    signum NullRisk = 1
    signum (R risk) = R (signum risk)
    fromInteger r = undefined
    negate r = undefined


input :: IO [[Int]]
input = map (map (\ c -> (read :: String -> Int) [c])) . lines <$> readFile "day_15_input.txt"

initCavern :: [[Int]] -> Cavern
initCavern = map (map (,NullRisk,NullCoord))

start :: RiskCoord
start = ((0,0), R 0)

maxCoords :: Cavern -> Coord
maxCoords c = (length (head c) -1, length c -1)

isWithinBounds :: Coord -> Coord -> Bool
isWithinBounds (maxX,maxY) (x,y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

neighbors :: Coord -> Cavern -> [Coord]
neighbors (x,y) c = filter (isWithinBounds (maxCoords c)) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

getElement :: Cavern -> Coord -> Element
getElement c (x,y) = c !! y !! x

updateElement :: Cavern -> Coord -> Element -> Cavern
updateElement cavern coord el = undefined

getRisk :: Element -> Risk
getRisk (r,_,_) = r

getRiskAsTotalRisk :: Element -> TotalRisk
getRiskAsTotalRisk = R . getRisk

getTotalRisk :: Element -> TotalRisk
getTotalRisk (_,tr,_) = tr

transitionIsCheaper :: RiskCoord -> Element -> Bool
transitionIsCheaper rc el =
  let currentRisk = snd rc
      newRisk = currentRisk + getRiskAsTotalRisk el
      oldRisk = getTotalRisk el
  in newRisk < oldRisk

updateRisk :: RiskCoord -> Element -> Element
updateRisk rc (r,tr,fc) = (r, snd rc + R r, fc)

possibleTransitions :: RiskCoord -> Cavern -> CheapestCoords
possibleTransitions coord cavern =
  let candidates = map (\ coord -> (coord, getElement cavern coord)) $ neighbors (fst coord) cavern
      possibleBeforeUpdatingRisk = filter (transitionIsCheaper coord . snd) candidates
      possible = map (second $ updateRisk coord) possibleBeforeUpdatingRisk
  in map (second getTotalRisk) possible

addToCheapestCoords :: CheapestCoords -> RiskCoord -> CheapestCoords
addToCheapestCoords [] rc = [rc]
addToCheapestCoords orig@(c:cs) rc = if snd rc < snd c then rc : orig else c : addToCheapestCoords cs rc

modifyCavern :: RiskCoord -> Cavern -> RiskCoord -> Cavern
modifyCavern latestCoord c rc =
    let coord = fst rc
    in helper coord latestCoord c rc
      where
        helper (x,0) latestCoord c rc =
          let row = head c
              begin = take x row
              toBeReplaced = row !! x
              rest = drop (x+1) row
          in (begin ++ (getRisk toBeReplaced, snd rc, C . fst $ latestCoord) : rest) : tail c
        helper (x,y) latestCoord c rc =
          let begin = take y c
              rest = drop y c
          in begin ++ helper (x,0) latestCoord rest rc

transitionsInOrder :: RiskCoord -> Cavern -> CheapestCoords -> (CheapestCoords,Cavern)
transitionsInOrder rc c chcs =
  let possible = possibleTransitions rc c
      newCheapest = foldl addToCheapestCoords chcs possible
      newCavern = foldl (modifyCavern rc) c possible
  in (newCheapest,newCavern)

step :: (CheapestCoords,Cavern) -> (CheapestCoords,Cavern)
step (chcs,c) = transitionsInOrder (head chcs) c (tail chcs)

endTotalRisk :: Cavern -> TotalRisk
endTotalRisk c = let endCoord = maxCoords c in getTotalRisk (getElement c endCoord)

backTrackPath :: Cavern -> [FromCoord]
backTrackPath c = let endCoord = maxCoords c in reverse $ helper ((\ (_,_,fc) -> fc) $ getElement c endCoord) c
  where
    helper NullCoord c = []
    helper orig@(C coord) c = if coord == (0,0) then [orig] else orig : helper ((\ (_,_,fc) -> fc) $ getElement c coord) c

safestPath :: Cavern -> [FromCoord]
safestPath c = helper ([start],c)
  where
    helper (chcs,c) =
      let etr = endTotalRisk c
      in if (snd . head $ chcs) < etr
           then helper $ step (chcs,c)
           else backTrackPath c

safestPathRisk :: Cavern -> Int
safestPathRisk c = helper ([start],c)
  where
    helper (chcs,c) =
      let etr = endTotalRisk c
      in if (snd . head $ chcs) < etr
           then helper $ step (chcs,c)
           else case etr of
                  R value -> value
                  NullRisk -> 0

firstPart :: IO Int
firstPart = safestPathRisk . initCavern <$> input

testInput :: [[Int]]
testInput = map (map (\ c -> (read :: String -> Int) [c])) ["1163751742", "1381373672", "2136511328", "3694931569", "7463417111", "1319128137", "1359912421", "3125421639", "1293138521", "2311944581"]
