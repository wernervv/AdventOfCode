import Data.Bifunctor(first,second)
import Data.List(group,minimumBy,partition,sortBy,sort)
import Data.Maybe(catMaybes,mapMaybe)

data BurrowSpace a = HW a | SR (a,a)
type BurrowUnit = (Int,BurrowSpace Char)
type Burrow = ([BurrowUnit],Int)
type Estimate = (Burrow,Int)
type Amphipod = (Char,Int)
type Dodges = [Amphipod]

data BurrowSpace2 a = H a | S (a,a,a,a)
type BurrowUnit2 = (Int, BurrowSpace2 Char)
type Burrow2 = ([BurrowUnit2],Int)

instance Show a => Show (BurrowSpace a) where
  show (HW val) = show val
  show (SR (val1,val2)) = "(" ++ show val1 ++ "," ++ show val2 ++ ")"

instance Show a => Show (BurrowSpace2 a) where
  show (H val) = show val
  show (S (v1,v2,v3,v4)) = "(" ++ show v1 ++ "," ++ show v2 ++ "," ++ show v3 ++ "," ++ show v4 ++ ")"

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

parseAsBurrow2 :: [String] -> Burrow2
parseAsBurrow2 = addMissingPart . parseAsBurrow

missing :: [(Char,Char)]
missing = [('D','D'),('C','B'),('B','A'),('A','C')]

addToRoom :: BurrowUnit -> [(Char,Char)] -> BurrowUnit2
addToRoom (x,HW val) _ = (x,H val)
addToRoom (x,SR (a,d)) ls = let (b,c) = head ls in (x,S (a,b,c,d))

afterOneUnit :: BurrowUnit -> [(Char,Char)] -> [(Char,Char)]
afterOneUnit (_,HW _) = id
afterOneUnit (_,SR _) = tail

listState :: [BurrowUnit] -> [[(Char,Char)]]
listState = (missing :) . scanl (flip afterOneUnit) missing

addMissingPart :: Burrow -> Burrow2
addMissingPart (lbu,count) = (zipWith addToRoom lbu (listState lbu),count)

lockedAmphipods :: Burrow -> [Amphipod]
lockedAmphipods = mapMaybe getAmphipod . hallWay

lockedAmphipods2 :: Burrow2 -> [Amphipod]
lockedAmphipods2 = mapMaybe getAmphipod2 . hallWay2

movableAmphipods :: Burrow -> [Amphipod]
movableAmphipods = mapMaybe getAmphipod . sideRooms

movableAmphipods2 :: Burrow2 -> [Amphipod]
movableAmphipods2 = mapMaybe getAmphipod2 . sideRooms2

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
  let as = movableAmphipods b ++ lockedAmphipods b
      aAndT = zip as (map (`getTargetRoomXCoord` b) as)
  in helper aAndT b
    where
      helper [] b = b
      helper ((a,x):rest) b =
        if snd a == x
          then helper rest b
          else case moveToGiven x a b of
                 Nothing -> helper rest b
                 (Just newB) -> allToTargets newB

allToTargets2 :: Burrow2 -> Burrow2
allToTargets2 b =
  let as = movableAmphipods2 b ++ lockedAmphipods2 b
      aAndT = zip as (map (`getTargetRoomXCoord2` b) as)
  in helper aAndT b
    where
      helper [] b = b
      helper ((a,x):rest) b =
        if snd a == x
          then helper rest b
          else case moveToGiven2 x a b of
                 Nothing -> helper rest b
                 (Just newB) -> allToTargets2 newB

-- branch :: Estimate -> Amphipod -> [Estimate]
-- branch (b,e) a = map tagWithEstimate . mapMaybe (\ (i,_) -> moveToGiven i a b) $ hallWay b

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

roomsContainingAmphipod :: Char -> Burrow -> [BurrowUnit]
roomsContainingAmphipod c = filter (\ (_,SR (c1,c2)) -> c1 == c || c2 == c) . sideRooms

stepsToOutsideOfTarget :: Int -> Char -> BurrowUnit -> Int
stepsToOutsideOfTarget target c (x,SR (c1,_)) =
  let toHallway = if c1 == c then 1 else 2
      distance = abs (target - x)
  in toHallway + distance
stepsToOutsideOfTarget _ _ _ = 0

hasCharOnBottom :: Char -> BurrowUnit -> Bool
hasCharOnBottom c (_,SR (_,c2)) = c == c2
hasCharOnBottom _ _ = False

absoluteMinimumCostChar :: Char -> Burrow -> Int
absoluteMinimumCostChar c b =
  let rooms = roomsContainingAmphipod c b
      target = getTargetRoomXCoord (c,0) b
      (inside,outside) = partition ((== target) . fst) rooms
  in case length inside of
       2 -> 0
       0 -> getMoveCost c . (3+) . sum . map (stepsToOutsideOfTarget target c) $ outside
       _ -> if hasCharOnBottom c (head inside)
              then getMoveCost c . (1+) . stepsToOutsideOfTarget target c . head $ outside
              else getMoveCost c . (3+) . sum . map (stepsToOutsideOfTarget target c) $ rooms

isInRightRoomWrongPos :: Char -> Burrow -> Bool
isInRightRoomWrongPos c b =
  let target = getTargetRoomXCoord (c,0) b
      (_,SR (c1,c2)) = head . filter (\ p -> fst p == target) $ sideRooms b
      res
        | c2 == c = False
        | c1 == c = True
        | otherwise = False
  in res

followLoop :: Int -> Char -> Burrow -> [Amphipod] -> [[Amphipod]]
followLoop 0 _ _ as = [as]
followLoop n c b as@(a:_) =
  let target = getTargetRoomXCoord a b
      (_,SR (c1,c2)) = head . filter (\ p -> fst p == target) $ sideRooms b
      branch1 = if c1 < c || c1 == fst a then [] else (c1,target) : as
      branch2 = if c2 < c || c2 == fst a then [] else (c2,target) : as
  in concatMap (followLoop (n-1) c b) [branch1,branch2]
followLoop _ _ _ _ = []

hasChar :: Char -> BurrowUnit -> Bool
hasChar c (_,SR (c1,c2)) = c1 == c || c2 == c
hasChar _ _ = False

getAmphipodsByChar :: Char -> Burrow -> (Amphipod,Amphipod)
getAmphipodsByChar c b =
  let roomsContaining = filter (hasChar c) $ sideRooms b
  in if length roomsContaining == 1
       then let x = fst . head $ roomsContaining in ((c,x),(c,x))
       else let x1 = fst . head $ roomsContaining
                x2 = fst . last $ roomsContaining
            in ((c,x1),(c,x2))

loopStartingPoints :: Int -> Char -> Burrow -> [Amphipod]
loopStartingPoints n c b =
  let (a1,a2) = getAmphipodsByChar c b
      firstStart = [a1 | any (\ ls -> length ls == (n+1) && head ls == last ls) (followLoop n c b [a1])]
      secondStart = [a2 | any (\ ls -> length ls == (n+1) && head ls == last ls) (followLoop n c b [a2])]
  in firstStart ++ secondStart

countDistinctLoops :: [(Bool,Int)] -> Int
countDistinctLoops = length . group . sort . filter fst

{-
dodgeScoreD :: Burrow -> Int
dodgeScoreD b = if isInRightRoomWrongPos 'D' b then getMoveCost 'D' 2 else 0

dodgeScoreC :: Burrow -> Int
dodgeScoreC b =
  let badPos = if isInRightRoomWrongPos 'C' b then 2 else 0
      stepsForBreakingLoops = (2*) . length . filter fst $ isPartOfLoop 2 'C' b
  in getMoveCost 'C' $ badPos + stepsForBreakingLoops

dodgeScoreB :: Burrow -> Int
dodgeScoreB b =
  let badPos = if isInRightRoomWrongPos 'B' b then 2 else 0
      twoLoops = isPartOfLoop 2 'B' b
      threeLoops = isPartOfLoop 3 'B' b
      stepsForBreakingLoops
        | (length . filter fst $ twoLoops) == 2 = 4
        | (length . filter fst $ threeLoops) == 2 = 4
        | otherwise = (2*) . countDistinctLoops $ twoLoops ++ threeLoops
  in getMoveCost 'B' $ badPos + stepsForBreakingLoops

dodgeScoreA :: Burrow -> Int
dodgeScoreA b =
  let badPos = if isInRightRoomWrongPos 'A' b then 2 else 0
      twoLoops = isPartOfLoop 2 'A' b
      threeLoops = isPartOfLoop 3 'A' b
      fourLoops = isPartOfLoop 4 'A' b
      stepsForBreakingLoops
        | (length . filter fst $ twoLoops) == 2 = 4
        | (length . filter fst $ threeLoops) == 2 = 4
        | (length . filter fst $ fourLoops) == 2 = 4
        | otherwise = (2*) . countDistinctLoops $ twoLoops ++ threeLoops ++ fourLoops
  in getMoveCost 'A' $ badPos + stepsForBreakingLoops
-}

countDodgesD :: Burrow -> [Amphipod]
countDodgesD b = [('D',getTargetRoomXCoord ('D',0) b) | isInRightRoomWrongPos 'D' b]

countDodgesC :: Burrow -> [Amphipod]
countDodgesC b =
  let badPos = [('C',getTargetRoomXCoord ('C',0) b) | isInRightRoomWrongPos 'C' b]
      dodgesForBreakingLoops = loopStartingPoints 2 'C' b
  in badPos ++ dodgesForBreakingLoops

countDodgesB :: Burrow -> [Amphipod]
countDodgesB b =
  let badPos = [('B',getTargetRoomXCoord ('B',0) b) | isInRightRoomWrongPos 'B' b]
      twoLoops = loopStartingPoints 2 'B' b
      threeLoops = loopStartingPoints 3 'B' b
      dodgesForBreakingLoops = twoLoops ++ threeLoops
  in badPos ++ dodgesForBreakingLoops

countDodgesA :: Burrow -> [Amphipod]
countDodgesA b =
  let badPos = [('A',getTargetRoomXCoord ('A',0) b) | isInRightRoomWrongPos 'A' b]
      twoLoops = loopStartingPoints 2 'A' b
      threeLoops = loopStartingPoints 3 'A' b
      fourLoops = loopStartingPoints 4 'A' b
      dodgesForBreakingLoops = twoLoops ++ threeLoops ++ fourLoops
  in badPos ++ dodgesForBreakingLoops

dodgeCount :: Burrow -> Dodges
dodgeCount b = countDodgesA b ++ countDodgesB b ++ countDodgesC b ++ countDodgesD b

-- dodgeScore :: Burrow -> Int
-- dodgeScore b = dodgeScoreD b + dodgeScoreC b + dodgeScoreB b + dodgeScoreA b

absoluteMinimumCost :: Burrow -> Int
absoluteMinimumCost b = sum . map (`absoluteMinimumCostChar` b) $ "ABCD"

-- smallestCost :: Burrow -> Int
-- smallestCost b = absoluteMinimumCost b + dodgeScore b

giveCheapest :: [Amphipod] -> Maybe Amphipod
giveCheapest [] = Nothing
giveCheapest as = Just . minimumBy (\ a b -> compare (fst a) (fst b)) $ as

giveSmallestDodgeCost :: Burrow -> Int
giveSmallestDodgeCost b =
  let range = relevantRange b
      cheapest = giveCheapest . filter (isInRange range) $ movableAmphipods b
  in maybe 0 ((`getMoveCost` 2) . fst) cheapest

-- countEstimate :: Burrow -> Int
-- countEstimate b =
--   let ghostScore = countGhostScore b
--       smallestDodge = giveSmallestDodgeCost b
--   in ghostScore + smallestDodge

-- transitions :: Estimate -> [Estimate]
-- transitions (b,e) =
--   let allMovedToTargets = allToTargets b
--       newEstimate = countEstimate allMovedToTargets
--       relevant = relevantAmphipods allMovedToTargets
--   in sortBy (\ a b -> compare (snd a) (snd b)) $ concatMap (branch (allMovedToTargets,newEstimate)) relevant

hallWay :: Burrow -> [BurrowUnit]
hallWay ([],_) = []
hallWay (b:bs,_) =
  case b of
    h@(_,HW _) -> h : hallWay (bs,0)
    _ -> hallWay (bs,0)

hallWay2 :: Burrow2 -> [BurrowUnit2]
hallWay2 ([],_) = []
hallWay2 (b:bs,_) =
  case b of
    h@(_,H _) -> h : hallWay2 (bs,0)
    _ -> hallWay2 (bs,0)

sideRooms :: Burrow -> [BurrowUnit]
sideRooms ([],_) = []
sideRooms (b:bs,_) =
  case b of
    s@(_,SR _) -> s : sideRooms (bs,0)
    _ -> sideRooms (bs,0)

sideRooms2 :: Burrow2 -> [BurrowUnit2]
sideRooms2 ([],_) = []
sideRooms2 (b:bs,_) =
  case b of
    s@(_,S _) -> s : sideRooms2 (bs,0)
    _ -> sideRooms2 (bs,0)

isInHallWay :: Burrow -> Amphipod -> Bool
isInHallWay b (_,x) = any (\ (pos,_) -> pos == x) $ hallWay b

isInHallWay2 :: Burrow2 -> Amphipod -> Bool
isInHallWay2 b (_,x) = any (\ (pos,_) -> pos == x) $ hallWay2 b

getTargetRoomXCoord :: Amphipod -> Burrow -> Int
getTargetRoomXCoord (c,_) b =
  let srCoords = map fst $ sideRooms b
      zipped = zip "ABCD" srCoords
  in snd . head $ filter (\ p -> fst p == c) zipped

getTargetRoomXCoord2 :: Amphipod -> Burrow2 -> Int
getTargetRoomXCoord2 (c,_) b =
  let sCoords = map fst $ sideRooms2 b
      zipped = zip "ABCD" sCoords
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

routeIsBlocked2 :: Int -> Int -> Burrow2 -> Bool
routeIsBlocked2 target current b =
  let h = hallWay2 b
      delta = current - target
      blockingBurrowUnits2
        | delta > 0 = filter (\ (x,_) -> let tmpDelta = x - target in tmpDelta >= 0 && tmpDelta < delta) h
        | current < target = filter (\ (x,_) -> let tmpDelta = x - target in tmpDelta <= 0 && tmpDelta > delta) h
        | otherwise = []
  in not . null . mapMaybe getAmphipod2 $ blockingBurrowUnits2

getSideRoom :: Int -> Burrow -> Maybe BurrowUnit
getSideRoom x (b,_) =
  let bu = head . filter (\ (n,_) -> n == x) $ b
  in case bu of
       val@(_,SR _) -> Just val
       _ -> Nothing

getSideRoom2 :: Int -> Burrow2 -> Maybe BurrowUnit2
getSideRoom2 x (b,_) =
  let bu = head . filter (\ (n,_) -> n == x) $ b
  in case bu of
       val@(_,S _) -> Just val
       _ -> Nothing

stepsToSR :: BurrowUnit -> Int
stepsToSR (_,HW _) = 0
stepsToSR (_,SR (_,c)) = if c == '.' then 2 else 1

stepsToS :: BurrowUnit2 -> Int
stepsToS (_,H _) = 0
stepsToS (_,S (_,c2,c3,c4))
  | c4 == '.' = 4
  | c3 == '.' = 3
  | c2 == '.' = 2
  | otherwise = 1

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

updateH :: BurrowSpace2 Char -> Char -> BurrowSpace2 Char
updateH = const H

updateSR :: BurrowSpace Char -> Char -> BurrowSpace Char
updateSR (SR (x,y)) c
  | c == '.' = if x == '.' then SR (x,c) else SR (c,y)
  | otherwise =
    if y == '.'
      then SR ('.',c)
      else SR (c,y)
updateSR _ _ = SR ('#','#')

updateS :: BurrowSpace2 Char -> Char -> BurrowSpace2 Char
updateS (S (a,b,c,d)) ch
  | ch == '.' = if a /= '.'
                  then S ('.',b,c,d)
                  else if b /= '.'
                         then S (a,'.',c,d)
                         else if c /= '.'
                                then S (a,b,'.',d)
                                else S (a,b,c,'.')
  | d == '.' = S (a,b,c,ch)
  | c == '.' = S (a,b,ch,d)
  | b == '.' = S (a,ch,c,d)
  | otherwise = S (ch,b,c,d)
updateS _ _ = S ('#','#','#','#')

updateBS :: BurrowSpace Char -> Char -> BurrowSpace Char
updateBS bs c = case bs of
                  hw@(HW _) -> updateHW hw c
                  sr@(SR _) -> updateSR sr c

updateBS2 :: BurrowSpace2 Char -> Char -> BurrowSpace2 Char
updateBS2 bs c = case bs of
                   h@(H _) -> updateH h c
                   s@(S _) -> updateS s c

updateTarget :: Int -> Char -> [BurrowUnit] -> [BurrowUnit]
updateTarget target c = map (\ orig@(x,bs) -> if x == target then (x,updateBS bs c) else orig)

updateTarget2 :: Int -> Char -> [BurrowUnit2] -> [BurrowUnit2]
updateTarget2 target c = map (\ orig@(x,bs) -> if x == target then (x,updateBS2 bs c) else orig)

makeMove :: Int -> Amphipod -> [BurrowUnit] -> [BurrowUnit]
makeMove target (c,x) = updateTarget x '.' . updateTarget target c

makeMove2 :: Int -> Amphipod -> [BurrowUnit2] -> [BurrowUnit2]
makeMove2 target (c,x) = updateTarget2 x '.' . updateTarget2 target c

cannotEnter :: Char -> BurrowUnit -> Bool
cannotEnter c (_,SR (c1,c2)) = let allowed = ['.',c] in notElem c1 allowed || notElem c2 allowed
cannotEnter _ (_,HW _) = False

cannotEnter2 :: Char -> BurrowUnit2 -> Bool
cannotEnter2 c (_,S (c1,c2,c3,c4)) = let allowed = ['.',c] in notElem c1 allowed || notElem c2 allowed || notElem c3 allowed || notElem c4 allowed
cannotEnter2 _ (_,H _) = False

cannotEnterTargetRoom :: Amphipod -> Int -> Burrow -> Bool
cannotEnterTargetRoom (c,_) target = cannotEnter c . head . filter (\ p -> fst p == target) . fst

cannotEnterTargetRoom2 :: Amphipod -> Int -> Burrow2 -> Bool
cannotEnterTargetRoom2 (c,_) target = cannotEnter2 c . head . filter (\ p -> fst p == target) . fst

getStepsToHallway :: Amphipod -> Burrow -> Int
getStepsToHallway a b =
  let sr = head . filter (\ p -> fst p == snd a) . fst $ b
  in case sr of
       (_,SR (c,_)) -> if c == '.' then 2 else 1
       (_,HW _) -> 0

getStepsToHallway2 :: Amphipod -> Burrow2 -> Int
getStepsToHallway2 a b =
  let s = head . filter (\ p -> fst p == snd a) . fst $ b
  in case s of
       (_,S (c1,c2,c3,_)) -> if c3 == '.'
                               then 4
                               else if c2 == '.'
                                      then 3
                                      else if c1 == '.'
                                             then 2
                                             else 1
       (_,H _) -> 0

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

moveToGiven2 :: Int -> Amphipod -> Burrow2 -> Maybe Burrow2
moveToGiven2 target a@(c,x) b@(bs,cost) =
  if routeIsBlocked2 target x b || cannotEnterTargetRoom2 a target b
    then Nothing
    else let mS = getSideRoom2 target b
             distance = abs (target - x) + maybe 0 stepsToS mS
             stepsToHallway = getStepsToHallway2 a b
             newCost = cost + getMoveCost c (stepsToHallway + distance)
             newBurrowUnits2 = makeMove2 target a bs
         in Just (newBurrowUnits2,newCost)

isTargetRoom :: BurrowUnit -> Amphipod -> Burrow -> Bool
isTargetRoom (x,_) a b = getTargetRoomXCoord a b == x

isTargetRoom2 :: BurrowUnit2 -> Amphipod -> Burrow2 -> Bool
isTargetRoom2 (x,_) a b = getTargetRoomXCoord2 a b == x

isHomogenous :: BurrowUnit -> Bool
isHomogenous (_,HW _) = False
isHomogenous (_,SR (a,b))
  | a == '.' = True
  | otherwise = a == b

isHomogenous2 :: BurrowUnit2 -> Bool
isHomogenous2 (_,H _) = False
isHomogenous2 (_,S (c1,c2,c3,c4)) = all (`elem` ['.',c4]) [c1,c2,c3]

alreadyCorrect :: Amphipod -> Burrow -> Bool
alreadyCorrect a b =
  let currentRoom = head . filter (\ p -> fst p == snd a) $ sideRooms b
  in isTargetRoom currentRoom a b && isHomogenous currentRoom

alreadyCorrect2 :: Amphipod -> Burrow2 -> Bool
alreadyCorrect2 a b =
  let currentRoom = head . filter (\ p -> fst p == snd a) $ sideRooms2 b
  in isTargetRoom2 currentRoom a b && isHomogenous2 currentRoom

allMoves :: Amphipod -> Burrow -> [Burrow]
allMoves a b =
  let targetRoom = getTargetRoomXCoord a b
      movedToTarget = moveToGiven targetRoom a b
  in if isInHallWay b a
       then case movedToTarget of Just val -> [val]; Nothing -> []
       else if alreadyCorrect a b
              then []
              else case movedToTarget of Just val -> [val]; Nothing -> mapMaybe ((\ x -> moveToGiven x a b) . fst) . hallWay $ b

allMoves2 :: Amphipod -> Burrow2 -> [Burrow2]
allMoves2 a b =
  let targetRoom = getTargetRoomXCoord2 a b
      movedToTarget = moveToGiven2 targetRoom a b
  in if isInHallWay2 b a
       then case movedToTarget of Just val -> [val]; Nothing -> []
       else if alreadyCorrect2 a b
              then []
              else case movedToTarget of Just val -> [val]; Nothing -> mapMaybe ((\ x -> moveToGiven2 x a b) . fst) . hallWay2 $ b

getAmphipod :: BurrowUnit -> Maybe Amphipod
getAmphipod (x,bs) =
  case bs of
    (HW c) -> if c == '.' then Nothing else Just (c,x)
    (SR (c1,c2)) -> if c1 == '.'
                      then if c2 == '.'
                             then Nothing
                             else Just (c2,x)
                      else Just (c1,x)

getAmphipod2 :: BurrowUnit2 -> Maybe Amphipod
getAmphipod2 (x,bs) =
  case bs of
    (H c) -> if c == '.' then Nothing else Just (c,x)
    (S (c1,c2,c3,c4)) -> if c1 == '.'
                           then if c2 == '.'
                                  then if c3 == '.'
                                         then if c4 == '.'
                                                then Nothing
                                                else Just (c4,x)
                                         else Just (c3,x)
                                  else Just (c2,x)
                           else Just (c1,x)

isFull :: BurrowUnit -> Bool
isFull (_,SR (c,_)) = c /= '.'
isFull _ = False

isFull2 :: BurrowUnit2 -> Bool
isFull2 (_,S (c1,_,_,_)) = c1 /= '.'
isFull2 _ = False

isEndState :: Burrow -> Bool
isEndState b = all (\ sr -> isHomogenous sr && isFull sr) $ sideRooms b

isEndState2 :: Burrow2 -> Bool
isEndState2 b = all (\ s -> isHomogenous2 s && isFull2 s) $ sideRooms2 b

addOneToEstimates :: [Estimate] -> Estimate -> [Estimate]
addOneToEstimates [] e = [e]
addOneToEstimates orig@(e:es) newE = if snd newE <= snd e then newE : orig else e : addOneToEstimates es newE

addToEstimates :: [Estimate] -> [Estimate] -> [Estimate]
addToEstimates = foldl addOneToEstimates

-- findCheaper :: Int -> [Estimate] -> Int
-- findCheaper cheapest [] = cheapest
-- findCheaper cheapest (e:es) =
--   if snd e >= cheapest
--     then cheapest
--     else findCheaper cheapest . addToEstimates es $ transitions e

-- cheapestEndState :: [Estimate] -> Int
-- cheapestEndState [] = maxBound
-- cheapestEndState ((b,e):rest) =
--   if isEndState b
--     then snd b
--     else cheapestEndState . addToEstimates rest $ transitions (b,e)

-- tagWithEstimate :: Burrow -> Estimate
-- tagWithEstimate b = (b,countEstimate b)

combineOne :: (Burrow,Dodges) -> [(Burrow,Dodges)] -> [(Burrow,Dodges)]
combineOne bd [] = [bd]
combineOne bd@((_,cost),_) (currentFirst@((_,currentCost),_):rest) =
  if cost <= currentCost
    then bd : currentFirst : rest
    else currentFirst : combineOne bd rest

combineStates :: [(Burrow,Dodges)] -> [(Burrow,Dodges)] -> [(Burrow,Dodges)]
combineStates toAdd current = foldl (flip combineOne) current toAdd

removeDodged :: Amphipod -> Dodges -> Dodges
removeDodged _ [] = []
removeDodged a (d:ds) = if d == a then ds else d : removeDodged a ds

nextStates :: Burrow -> Dodges -> [(Burrow,Dodges)]
nextStates b ds =
  let (small,big) = relevantRange b
      movable = filter (`elem` ds) $ relevantAmphipods b
      allMoves = concatMap (\ a -> zip (mapMaybe (\ i -> moveToGiven i a b) [small..big]) (repeat $ removeDodged a ds)) movable
  in map (first allToTargets) allMoves

costOfEndState :: [(Burrow,Dodges)] -> Int
costOfEndState [] = maxBound
costOfEndState ((b,ds):rest) =
  if isEndState b
    then snd b
    else costOfEndState . combineStates (nextStates b ds) $ rest

smallestEnd :: Burrow -> Int
smallestEnd b =
  let dodges = dodgeCount b
  in costOfEndState [(b,dodges)]

firstPart :: IO Int
firstPart = smallestEnd . parseAsBurrow <$> input

nextStates2 :: Burrow2 -> [Burrow2]
nextStates2 b = concatMap (`allMoves2` b) (movableAmphipods2 b)

branches :: Burrow2 -> [Burrow2] -> [Burrow2]
branches b bs = map allToTargets2 (nextStates2 b) ++ bs

findCheaper :: Int -> [Burrow2] -> Int
findCheaper cheapest [] = cheapest
findCheaper cheapest (b:bs) =
  if isEndState2 b
    then findCheaper (min cheapest (snd b)) bs
    else findCheaper cheapest $ branches b bs

findCheapestEndState :: [Burrow2] -> Int
findCheapestEndState [] = maxBound
findCheapestEndState (b:bs) =
  if isEndState2 b
    then findCheaper (snd b) bs
    else findCheapestEndState $ branches b bs

cheapestEndStateCost :: Burrow2 -> Int
cheapestEndStateCost b = findCheapestEndState [b]

secondPart :: IO Int
secondPart = cheapestEndStateCost . parseAsBurrow2 <$> input

i1 :: [String]
i1 = ["#############"
     ,"#...........#"
     ,"###D#B#B#A###"
     ,"  #C#C#D#A#"
     ,"  #########"]

{-
              B5
     B7       C5
2A9       D3
2A9  D7   C3
-}

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
B3 : -  : C5 D5
C5 : -  : B7
D5 : C5 : D9 A9
B7 : -  : C5 D5
D9 : -  : A9
A9 : D9 : B3

B7 -> B4

// B3 : -  : C5 D5
B4 : +  : C5 D5
C5 : -  : -
D5 : C5 : D9 A9
D9 : -  : A9
A9 : D9 : B3

C5 -> C7

// B3 : -  : D5
B4 : +  : D5
D5 : -  : D9 A9
D9 : -  : A9
A9 : D9 : B3

D9 -> D8

// B3 : -  : D5
// B4 : +  : D5
// D5 : -  : A9
D8 : +  : A9
A9 : -  : B3

A9 -> A10

// B3 : -  : D5
// B4 : +  : D5
// D5 : -  : -
D8  : +  : -
A10 : +  : B3

D8 -> D9

// B3 : -  : D5
B4  : +  : D5
D5  : -  : -
A10 : +  : B3

D5 -> D9

// B3 : -  : -
B4  : +  : -
A10 : +  : B3

B4 -> B5

B3 : -  : -
A10 : +  : B3

B3 -> B5

A10 : + : -

A10 -> A3

-}

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
