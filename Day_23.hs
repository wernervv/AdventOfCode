import Data.Maybe(catMaybes,mapMaybe)

data BurrowSpace a = HW a | SR (a,a)
type BurrowUnit = (Int,BurrowSpace Char)
type Burrow = ([BurrowUnit],Int)
type Amphipod = (Char,Int)

instance Show a => Show (BurrowSpace a) where
  show (HW val) = show val
  show (SR (val1,val2)) = "(" ++ show val1 ++ "," ++ show val2 ++ ")"

input :: IO [String]
input = lines <$> readFile "day_23_input.txt"

parseAsBurrow :: [String] -> Burrow
parseAsBurrow ls =
  let [hw,sr1,sr2] = tail . init $ ls
      appendedSr2 = sr2 ++ repeat '#'
      zipped = zip (zip (zip hw sr1) appendedSr2) [0..]
  in (concatMap tupleIntoBurrowUnitL zipped, 0)
       where
         tupleIntoBurrowUnitL (((a,b),c),n)
           | b `elem` "ABCD" = [(n, SR (b,c))]
           | b == '#' && a == '.' = [(n,HW a)]
           | otherwise = []

hallWay :: Burrow -> [BurrowUnit]
hallWay ([],_) = []
hallWay (b:bs,_) =
  case b of
    h@(_,HW _) -> h : hallWay (bs,0)
    _ -> hallWay (bs,0)

sideRooms :: Burrow -> [BurrowUnit]
sideRooms ([],_) = []
sideRooms (b:bs,_) =
  case b of
    s@(_,SR _) -> s : sideRooms (bs,0)
    _ -> sideRooms (bs,0)

isInHallWay :: Burrow -> Amphipod -> Bool
isInHallWay b (_,x) = any (\ (pos,_) -> pos == x) $ hallWay b

getTargetRoomXCoord :: Amphipod -> Burrow -> Int
getTargetRoomXCoord (c,_) b =
  let srCoords = map fst $ sideRooms b
      zipped = zip "ABCD" srCoords
  in snd . head $ filter (\ p -> fst p == c) zipped

routeIsBlocked :: Int -> Int -> Burrow -> Bool
routeIsBlocked target current b =
  let hw = hallWay b
      delta = current - target
      blockingBurrowUnits
        | delta > 0 = filter (\ (x,_) -> let tmpDelta = x - target in tmpDelta >= 0 && tmpDelta < delta) hw
        | current < target = filter (\ (x,_) -> let tmpDelta = x - target in tmpDelta <= 0 && tmpDelta > delta) hw
        | otherwise = []
  in not . null . mapMaybe getAmphipod $ blockingBurrowUnits

getSideRoom :: Int -> Burrow -> Maybe BurrowUnit
getSideRoom x (b,_) =
  let bu = head . filter (\ (n,_) -> n == x) $ b
  in case bu of
       val@(_,SR _) -> Just val
       _ -> Nothing

stepsToSR :: BurrowUnit -> Int
stepsToSR (_,HW _) = 0
stepsToSR (_,SR (_,c)) = if c == '.' then 2 else 1

getMoveCost :: Char -> Int -> Int
getMoveCost c n =
  let unitCost =
        case c of
          'A' -> 1
          'B' -> 10
          'C' -> 100
          'D' -> 1000
          _   -> 0
  in n * unitCost

updateHW :: BurrowSpace Char -> Char -> BurrowSpace Char
updateHW = const HW

updateSR :: BurrowSpace Char -> Char -> BurrowSpace Char
updateSR (SR (_,x)) c =
  if x == '.'
    then SR ('.',c)
    else SR (c,x)
updateSR _ _ = SR ('#','#')

updateBS :: BurrowSpace Char -> Char -> BurrowSpace Char
updateBS bs c = case bs of
                  hw@(HW _) -> updateHW hw c
                  sr@(SR _) -> updateSR sr c

updateTarget :: Int -> Char -> [BurrowUnit] -> [BurrowUnit]
updateTarget target c = map (\ orig@(x,bs) -> if x == target then (x,updateBS bs c) else orig)

makeMove :: Int -> Amphipod -> [BurrowUnit] -> [BurrowUnit]
makeMove target (c,x) = updateTarget x '.' . updateTarget target c

moveToGiven :: Int -> Amphipod -> Burrow -> Maybe Burrow
moveToGiven target a@(c,x) b@(bs,cost) =
  if routeIsBlocked target x b
    then Nothing
    else let mSR = getSideRoom target b
             distance = abs (target - x) + maybe 0 stepsToSR mSR
             newCost = cost + getMoveCost c distance
             newBurrowUnits = makeMove target a bs
         in Just (newBurrowUnits,newCost)

isTargetRoom :: BurrowUnit -> Amphipod -> Burrow -> Bool
isTargetRoom (x,_) a b = getTargetRoomXCoord a b == x

isHomogenous :: BurrowUnit -> Bool
isHomogenous (_,HW _) = False
isHomogenous (_,SR (a,b))
  | a == '.' = True
  | otherwise = a == b

alreadyCorrect :: Amphipod -> Burrow -> Bool
alreadyCorrect a b =
  let currentRoom = head . filter (\ p -> fst p == snd a) $ sideRooms b
  in isTargetRoom currentRoom a b && isHomogenous currentRoom

allMoves :: Amphipod -> Burrow -> [Burrow]
allMoves a b =
  let targetRoom = getTargetRoomXCoord a b
      movedToTarget = moveToGiven targetRoom a b
  in if isInHallWay b a
       then case movedToTarget of Just val -> [val]; Nothing -> []
       else if alreadyCorrect a b
              then []
              else let movesToHallWay = map ((\ x -> moveToGiven x a b) . fst) . hallWay $ b
                   in catMaybes $ movedToTarget : movesToHallWay

getAmphipod :: BurrowUnit -> Maybe Amphipod
getAmphipod (x,bs) =
  case bs of
    (HW c) -> if c == '.' then Nothing else Just (c,x)
    (SR (c1,c2)) -> if c1 == '.'
                      then if c2 == '.'
                             then Nothing
                             else Just (c2,x)
                      else Just (c1,x)

possibleTransitions :: Burrow -> [Burrow]
possibleTransitions burrow = concat . mapMaybe (fmap (`allMoves` burrow) . getAmphipod) . fst $ burrow

getTotalEnergySpent :: Burrow -> Int
getTotalEnergySpent = snd

isEndState :: Burrow -> Bool
isEndState b = all isHomogenous $ sideRooms b

addOneToBurrows :: [Burrow] -> Burrow -> [Burrow]
addOneToBurrows [] b = [b]
addOneToBurrows orig@(b:bs) newB = if snd newB <= snd b then newB : orig else b : addOneToBurrows bs newB

addToBurrows :: [Burrow] -> [Burrow] -> [Burrow]
addToBurrows = foldl addOneToBurrows

cheapestEndState :: [Burrow] -> Int
cheapestEndState [] = maxBound
cheapestEndState (b:rest) =
  if isEndState b
    then let currentCheapest = getTotalEnergySpent b in min currentCheapest (cheapestEndState rest)
    else let nextStates = possibleTransitions b in cheapestEndState $ addToBurrows rest nextStates

firstPart :: IO Int
firstPart = cheapestEndState . (:[]) . parseAsBurrow <$> input
