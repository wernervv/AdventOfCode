import Data.List(minimumBy,sortBy)
import Data.Maybe(catMaybes,mapMaybe)

data BurrowSpace a = HW a | SR (a,a)
type BurrowUnit = (Int,BurrowSpace Char)
type Burrow = ([BurrowUnit],Int)
type Estimate = (Burrow,Int)
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

lockedAmphipods :: Burrow -> [Amphipod]
lockedAmphipods = mapMaybe getAmphipod . hallWay

movableAmphipods :: Burrow -> [Amphipod]
movableAmphipods = mapMaybe getAmphipod . sideRooms

constrictOne :: (Int,Int) -> Amphipod -> Burrow -> (Int,Int)
constrictOne (small,big) a@(_,x) b =
  let t = getTargetRoomXCoord a b
  in if t <= x then (small,x-1) else (x+1,big)

constrictRange :: (Int,Int) -> [Amphipod] -> Burrow -> (Int,Int)
constrictRange r as b = foldl (\ range a -> constrictOne range a b) r as

isInRange :: (Int,Int) -> Amphipod -> Bool
isInRange (small,big) (_,x) = x >= small && x <= big

relevantRange :: Burrow -> (Int,Int)
relevantRange b =
  let blocking = lockedAmphipods b
  in constrictRange (1,11) blocking b

relevantAmphipods :: Burrow -> [Amphipod]
relevantAmphipods b = filter (\ a -> not (alreadyCorrect a b) && isInRange (relevantRange b) a) $ movableAmphipods b

allToTargets :: Burrow -> Burrow
allToTargets b =
  let as = movableAmphipods b
      aAndT = zip as (map (`getTargetRoomXCoord` b) as)
  in helper aAndT b
    where
      helper [] b = b
      helper ((a,x):rest) b =
        case moveToGiven x a b of
          Nothing -> helper rest b
          (Just newB) -> allToTargets newB

branch :: Estimate -> Amphipod -> [Estimate]
branch (b,e) a = map tagWithEstimate . mapMaybe (\ (i,_) -> moveToGiven i a b) $ hallWay b

allAmphipods :: [BurrowUnit] -> [Amphipod]
allAmphipods [] = []
allAmphipods (b:bs) =
  case b of
    (x,HW c) -> if c == '.' then allAmphipods bs else (c,x) : allAmphipods bs
    (x,SR (c1,c2)) -> if c1 /= '.' then (c1,x) : (c2,x) : allAmphipods bs else if c2 /= '.' then (c2,x) : allAmphipods bs else allAmphipods bs

scoreToSpanHorizontalDistance :: Amphipod -> Burrow -> Int
scoreToSpanHorizontalDistance a@(c,x) b =
  let t = getTargetRoomXCoord a b
  in getMoveCost c $ abs (t - x)

countGhostScore :: Burrow -> Int
countGhostScore b = (4444 +) . sum . map (`scoreToSpanHorizontalDistance` b) $ allAmphipods (fst b)

giveCheapest :: [Amphipod] -> Maybe Amphipod
giveCheapest [] = Nothing
giveCheapest as = Just . minimumBy (\ a b -> compare (fst a) (fst b)) $ as

giveSmallestDodgeCost :: Burrow -> Int
giveSmallestDodgeCost b =
  let range = relevantRange b
      cheapest = giveCheapest . filter (isInRange range) $ movableAmphipods b
  in maybe 0 ((`getMoveCost` 2) . fst) cheapest

countEstimate :: Burrow -> Int
countEstimate b =
  let ghostScore = countGhostScore b
      smallestDodge = giveSmallestDodgeCost b
  in ghostScore + smallestDodge

transitions :: Estimate -> [Estimate]
transitions (b,e) =
  let allMovedToTargets = allToTargets b
      newEstimate = countEstimate allMovedToTargets
      relevant = relevantAmphipods allMovedToTargets
  in sortBy (\ a b -> compare (snd a) (snd b)) $ concatMap (branch (allMovedToTargets,newEstimate)) relevant

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
updateSR (SR (x,y)) c
  | c == '.' = if x == '.' then SR (x,c) else SR (c,y)
  | otherwise =
    if y == '.'
      then SR ('.',c)
      else SR (c,y)
updateSR _ _ = SR ('#','#')

updateBS :: BurrowSpace Char -> Char -> BurrowSpace Char
updateBS bs c = case bs of
                  hw@(HW _) -> updateHW hw c
                  sr@(SR _) -> updateSR sr c

updateTarget :: Int -> Char -> [BurrowUnit] -> [BurrowUnit]
updateTarget target c = map (\ orig@(x,bs) -> if x == target then (x,updateBS bs c) else orig)

makeMove :: Int -> Amphipod -> [BurrowUnit] -> [BurrowUnit]
makeMove target (c,x) = updateTarget x '.' . updateTarget target c

cannotEnter :: Char -> BurrowUnit -> Bool
cannotEnter c (_,SR (c1,c2)) = let allowed = ['.',c] in notElem c1 allowed || notElem c2 allowed
cannotEnter _ (_,HW _) = False

cannotEnterTargetRoom :: Amphipod -> Int -> Burrow -> Bool
cannotEnterTargetRoom (c,_) target = cannotEnter c . head . filter (\ p -> fst p == target) . fst

getStepsToHallway :: Amphipod -> Burrow -> Int
getStepsToHallway a b =
  let sr = head . filter (\ p -> fst p == snd a) . fst $ b
  in case sr of
       (_,SR (c,_)) -> if c == '.' then 2 else 1
       (_,HW _) -> 0

moveToGiven :: Int -> Amphipod -> Burrow -> Maybe Burrow
moveToGiven target a@(c,x) b@(bs,cost) =
  if routeIsBlocked target x b || cannotEnterTargetRoom a target b
    then Nothing
    else let mSR = getSideRoom target b
             distance = abs (target - x) + maybe 0 stepsToSR mSR
             stepsToHallway = getStepsToHallway a b
             newCost = cost + getMoveCost c (stepsToHallway + distance)
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
              else case movedToTarget of Just val -> [val]; Nothing -> mapMaybe ((\ x -> moveToGiven x a b) . fst) . hallWay $ b

getAmphipod :: BurrowUnit -> Maybe Amphipod
getAmphipod (x,bs) =
  case bs of
    (HW c) -> if c == '.' then Nothing else Just (c,x)
    (SR (c1,c2)) -> if c1 == '.'
                      then if c2 == '.'
                             then Nothing
                             else Just (c2,x)
                      else Just (c1,x)


isFull :: BurrowUnit -> Bool
isFull (_,SR (c,_)) = c /= '.'
isFull _ = False

isEndState :: Burrow -> Bool
isEndState b = all (\ sr -> isHomogenous sr && isFull sr) $ sideRooms b

addOneToEstimates :: [Estimate] -> Estimate -> [Estimate]
addOneToEstimates [] e = [e]
addOneToEstimates orig@(e:es) newE = if snd newE <= snd e then newE : orig else e : addOneToEstimates es newE

addToEstimates :: [Estimate] -> [Estimate] -> [Estimate]
addToEstimates = foldl addOneToEstimates

findCheaper :: Int -> [Estimate] -> Int
findCheaper cheapest [] = cheapest
findCheaper cheapest (e:es) =
  if snd e >= cheapest
    then cheapest
    else findCheaper cheapest . addToEstimates es $ transitions e

cheapestEndState :: [Estimate] -> Int
cheapestEndState [] = maxBound
cheapestEndState ((b,e):rest) =
  if isEndState b
    then snd b
    else cheapestEndState . addToEstimates rest $ transitions (b,e)

tagWithEstimate :: Burrow -> Estimate
tagWithEstimate b = (b,countEstimate b)

firstPart :: IO Int
firstPart = cheapestEndState . (:[]) . tagWithEstimate . parseAsBurrow <$> input

i1 :: [String]
i1 = ["#############"
     ,"#...........#"
     ,"###D#B#B#A###"
     ,"  #C#C#D#A#"
     ,"  #########"]

{-
D3 : 2A9
C3 : B7 D7
B5 : C5
C5 : B7 D7
B7 : B5 C5
D7 : 2A9
2A9 : D3 C3
-}

{-
move to target
dodge
  clearly bad dodge (forces many / expensive dodges) -> don't do
  maybe good dodge -> see what happens
-}

testInput :: [String]
testInput = ["#############"
            ,"#...........#"
            ,"###B#C#B#D###"
            ,"  #A#D#C#A#"
            ,"  #########"]

{-
B3 : C5 D5
A3 -
C5 : B7
D5 : (D9) A9
B7 : C5 D5
C7 -
D9 : A9
A9 : B3 D9
-}

-- B7 -> B4

m1 :: [String]
m1 = ["#############"
     ,"#...B.......#"
     ,"###B#C#.#D###"
     ,"  #A#D#C#A#"
     ,"  #########"]

{-
B3 : C5 D5 B4
A3 -
B4 : C5 D5
C5 *
D5 : (D9) A9
C7 -
D9 : A9
A9 : B3 B4 D9
-}

-- C5 -> C7

m2 :: [String]
m2 = ["#############"
     ,"#...B.......#"
     ,"###B#.#C#D###"
     ,"  #A#D#C#A#"
     ,"  #########"]

{-
B3 : D5 B4
A3 -
B4 : D5
D5 : (D9) A9
2C7 -
D9 : A9
A9 : B3 B4 D9
-}

-- D5 -> D6

m3 :: [String]
m3 = ["#############"
     ,"#...B.D.....#"
     ,"###B#.#C#D###"
     ,"  #A#.#C#A#"
     ,"  #########"]

{-
B3 : B4
A3 -
B4 *
D6 : (D9) A9
2C7 -
D9 : A9
A9 : B3 B4 D6 D9
-}

-- B4 -> B5

m4 :: [String]
m4 = ["#############"
     ,"#.....D.....#"
     ,"###B#.#C#D###"
     ,"  #A#B#C#A#"
     ,"  #########"]

{-
B3 *
A3 -
B5 -
D6 : (D9) A9
2C7 -
D9 : A9
A9 : B3 D6 D9
-}

-- B3 -> B5

m5 :: [String]
m5 = ["#############"
     ,"#.....D.....#"
     ,"###.#B#C#D###"
     ,"  #A#B#C#A#"
     ,"  #########"]

{-
A3 -
2B5 -
D6 : (D9) A9
2C7 -
D9 : A9
A9 : D6 D9
-}

-- D9 -> D8

m6 :: [String]
m6 = ["#############"
     ,"#.....D.D...#"
     ,"###.#B#C#.###"
     ,"  #A#B#C#A#"
     ,"  #########"]

{-
A3 -
2B5 -
D6 : D8 A9
2C7 -
D8 : A9
A9 : D6 D8
-}

-- A9 -> A10

m7 :: [String]
m7 = ["#############"
     ,"#.....D.D.A.#"
     ,"###.#B#C#.###"
     ,"  #A#B#C#.#"
     ,"  #########"]

{-
A3 -
2B5 -
D6 : D8
2C7 -
D8 *
A10 : D6 D8
-}

-- D8 -> D9

m8 :: [String]
m8 = ["#############"
     ,"#.....D...A.#"
     ,"###.#B#C#.###"
     ,"  #A#B#C#D#"
     ,"  #########"]

{-
A3 -
2B5 -
D6 *
2C7 -
D9 -
A10 : D6
-}

-- D6 -> D9

m9 :: [String]
m9 = ["#############"
     ,"#.........A.#"
     ,"###.#B#C#D###"
     ,"  #A#B#C#D#"
     ,"  #########"]

{-
A3 -
2B5 -
2C7 -
2D9 -
A10 *
-}

m10 :: [String]
m10 = ["#############"
     ,"#...........#"
     ,"###A#B#C#D###"
     ,"  #A#B#C#D#"
     ,"  #########"]

{-
2A3 -
2B5 -
2C7 -
2D9 -
-}

-- D 12, C 4, B 11, A 11
-- D  8, C 4, B  9, A 9

nearlyFinished :: [String]
nearlyFinished = ["#############"
                 ,"#.....D.D.A.#"
                 ,"###.#B#C#.###"
                 ,"  #A#B#C#.#"
                 ,"  #########"]

{-
is correct -> do nothing
  can move to target room -> move there
    something is blocking the path in hallway -> they should move first
    the target room has other amphipods in it ->
      current amphipod is not blocking / inhabiting their target room -> they should move first
      is blocking (in the hallway) -> they should move first (dodge)
      is blocking in same side room -> move first
      is inhabiting target room (need to cross one another) -> make cheaper dodge
-}
