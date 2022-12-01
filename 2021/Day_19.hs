import Data.Bifunctor(bimap)
import Data.List(sortBy,union)
import Data.List.Split(splitOn)
import Data.Maybe(catMaybes,fromMaybe)

type Coord = (Int,Int,Int)
type QuadCoord = (Coord,Coord,Coord,Coord)
data AxisGeneral a = P a | N a
type Axes = (AxisGeneral Char, AxisGeneral Char, AxisGeneral Char)
type Instruction = (Axes,Coord)

instance Eq a=> Eq (AxisGeneral a) where
  P val1 == P val2 = val1 == val2
  N val1 == N val2 = val1 == val2
  _ == _ = False

instance Show a => Show (AxisGeneral a) where
  show (P val) = show val
  show (N val) = '-' : show val

input :: IO [[Coord]]
input = map (map readCoord . tail) . splitOn [""] . lines <$> readFile "day_19_input.txt"

readCoord :: String -> Coord
readCoord = (\ [x,y,z] -> (x,y,z)) . map (read :: String -> Int) . splitOn ","

allDifferentPairs :: [a] -> [(a,a)]
allDifferentPairs ls =
  let n = length ls -1
  in [(ls !! x, ls !! y) | x <- [0..n], y <- [x..n], x /= y]

calculateDistance :: Coord -> Coord -> Float
calculateDistance (x1,y1,z1) (x2,y2,z2) = sqrt . fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

epsilon :: Float
epsilon = 0.01

distancesAreSame :: Float -> Float -> Bool
distancesAreSame f1 f2 = abs (f1 - f2) < epsilon

collectMatching :: [((Coord,Coord),Float)] -> [((Coord,Coord),Float)] -> [(Coord,Coord,Coord,Coord)]
collectMatching [] _ = []
collectMatching _ [] = []
collectMatching (((a1,a2),d1):as) (((b1,b2),d2):bs)
  | distancesAreSame d1 d2 = (a1,a2,b1,b2) : collectMatching as bs
  | d1 < d2 = collectMatching as (((b1,b2),d2):bs)
  | d1 > d2 = collectMatching (((a1,a2),d1):as) bs
  | otherwise = []

pairsByDistance :: [Coord] -> [Coord] -> [QuadCoord]
pairsByDistance as bs =
  let allAPairs = allDifferentPairs as
      allBPairs = allDifferentPairs bs
      sortedDistancesA = sortBy (\ (_,d1) (_,d2) -> d1 `compare` d2) $ map (\ coords -> (coords, uncurry calculateDistance coords)) allAPairs
      sortedDistancesB = sortBy (\ (_,d1) (_,d2) -> d1 `compare` d2) $ map (\ coords -> (coords, uncurry calculateDistance coords)) allBPairs
  in collectMatching sortedDistancesA sortedDistancesB

hasCoord :: QuadCoord -> Coord -> Bool
hasCoord (a1,a2,b1,b2) c = c == a1 || c == a2 || c == b1 || c == b2

isPresentBInBoth :: Coord -> [QuadCoord] -> Bool
isPresentBInBoth c = all (\ (_,_,b1,b2) -> c == b1 || c == b2)

theOtherPair :: (Coord,Coord) -> QuadCoord -> (Coord,Coord)
theOtherPair (c1,c2) (a1,a2,b1,b2) = (otherA,otherB)
  where
    otherA = if c1 == a1 then a2 else a1
    otherB = if c2 == b1 then b2 else b1

matchRest :: (Coord,Coord) -> [QuadCoord] -> [(Coord,Coord)]
matchRest firstPair = map (theOtherPair firstPair)

matchingBeacons :: [QuadCoord] -> [(Coord,Coord)]
matchingBeacons [] = []
matchingBeacons (qc:rest) =
  let (a1,a2,b1,b2) = qc
      allContainingFirst = filter (`hasCoord` a1) rest
      (matchingB,notMatchingB) = if isPresentBInBoth b1 (take 2 allContainingFirst) then (b1,b2) else (b2,b1)
      firstPair = (a1,matchingB)
      secondPair = (a2,notMatchingB)
  in firstPair : secondPair : matchRest firstPair allContainingFirst

allScannerPairs :: [[Coord]] -> [(Int,Int)]
allScannerPairs input =
  let allPairs = allDifferentPairs $ zip input [0..]
      allScanningSameArea = filter (\ p -> length (fst p) >= 12) . map (\ ((lc1,id1),(lc2,id2)) -> (matchingBeacons $ pairsByDistance lc1 lc2, (id1,id2))) $ allPairs
  in map snd allScanningSameArea

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f

getAxisInfo :: AxisGeneral Char -> (Char,Bool)
getAxisInfo (P val) = (val, True)
getAxisInfo (N val) = (val, False)

axesInfo :: Axes -> ((Char,Bool),(Char,Bool),(Char,Bool))
axesInfo = allThree getAxisInfo

-- deduceLastAxis :: ((Char,Bool),(Char,Bool)) -> ((Char,Bool),(Char,Bool),(Char,Bool))
-- deduceLastAxis ((x,xPositive),(y,yPositive)) =
--   let z = head $ filter (`notElem` [x,y]) "xyz"
--       zPositive = xPositive == yPositive
--   in ((x,xPositive),(y,yPositive),(z,zPositive))

allThree :: (a -> b) -> (a,a,a) -> (b,b,b)
allThree f (x,y,z) = (f x, f y, f z)

rotateOne :: (Char,Bool) -> Coord -> Int
rotateOne (axis,isPositive) = signFunction . pickFunction
  where
    signFunction = if isPositive then id else negate
    pickFunction
      | axis == 'x' = \ (x,_,_) -> x
      | axis == 'y' = \ (_,y,_) -> y
      | otherwise   = \ (_,_,z) -> z

rotateCoord :: Axes -> Coord -> Coord
rotateCoord a c =
  let (x,y,z) = allThree getAxisInfo a
  in allThree (`rotateOne` c) (x,y,z)

-- rotateCoord :: Axes -> Coord -> Coord
-- rotateCoord a c =
--   let (x,y,z) = deduceLastAxis . axesInfo $ a
--   in allThree (`rotateOne` c) (x,y,z)

coordPlus :: Coord -> Coord -> Coord
coordPlus (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

moveCoord :: Coord -> Coord -> Coord
moveCoord = coordPlus

translateCoord :: Instruction -> Coord -> Coord
translateCoord (a,o) = moveCoord o . rotateCoord a

negateAxis :: AxisGeneral a -> AxisGeneral a
negateAxis (P val) = N val
negateAxis (N val) = P val

allRotations :: [Axes]
allRotations = [(x, y, z) | x <- allAxes, y <- allAxes, x /= y, z <- allAxes, y /= z]
  where
    allAxes = [P 'x',N 'x',P 'y',N 'y',P 'z',N 'z']

-- allRotations :: [Axes]
-- allRotations = [(x,y) | x <- allAxes, y <- allAxes, x /= y, x /= negateAxis y]
--   where
--     allAxes = [P 'x',N 'x',P 'y',N 'y',P 'z',N 'z']

coordMinus :: Coord -> Coord -> Coord
coordMinus (x1,y1,z1) (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

instructionsToMatchFirst :: [(Coord,Coord)] -> Instruction
instructionsToMatchFirst beacons =
  let subset = take 2 beacons
      aCoords = map fst subset
      bCoords = map snd subset
      allRotatedBWithAxes = map (\ a -> map (\ c -> (rotateCoord a c, a)) bCoords) allRotations
      allABPairs = zip (repeat aCoords) allRotatedBWithAxes
      mAxesAndOffset = map (\ ([a1,a2],[(b1,ax),(b2,_)]) -> if a1 `coordMinus` b1 == a2 `coordMinus` b2 then Just (ax,a1 `coordMinus` b1) else Nothing) allABPairs
  in head (catMaybes mAxesAndOffset)

instructionToTranslateSecond :: [Coord] -> [Coord] -> Instruction
instructionToTranslateSecond a b = instructionsToMatchFirst . matchingBeacons $ pairsByDistance a b

instructionsCoverAll :: [(Instruction,Int)] -> [(Int,Int)] -> Bool
instructionsCoverAll instr unchecked =
  let instrNumbers = map snd instr
  in all (\ (a,b) -> a `elem` instrNumbers && b `elem` instrNumbers) unchecked

translateAll :: [(Instruction,Int)] -> [[Coord]] -> [[Coord]]
translateAll instructionsByID input = map (\ (i,ind) -> map (translateCoord i) $ input !! ind) instructionsByID

isNotFound :: [(Instruction,Int)] -> (Int,Int) -> Bool
isNotFound instructionsByID (a,b) =
  let instrNumbers = map snd instructionsByID
  in not $ a `elem` instrNumbers || b `elem` instrNumbers

findOverlapping :: [(Instruction,Int)] -> [(Int,Int)] -> Maybe (Int,Int)
findOverlapping _ [] = Nothing
findOverlapping instructionsByID ((a,b):rest)
  | a `elem` map snd instructionsByID = if b `elem` map snd instructionsByID then findOverlapping instructionsByID rest else Just (a,b)
  | b `elem` map snd instructionsByID = Just (b,a)
  | otherwise = findOverlapping instructionsByID rest

removeGivenElement :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
removeGivenElement _ [] = []
removeGivenElement (a1,b1) (currentFirst@(a2,b2):rest) =
  if (a1 == a2 && b1 == b2) || (a1 == b2 && b1 == a2)
    then rest
    else currentFirst : removeGivenElement (a1,b1) rest

moreInstructions :: [(Instruction,Int)] -> [(Int,Int)] -> [[Coord]] -> ([(Instruction,Int)],[(Int,Int)])
moreInstructions instructionsByID unchecked input =
  let mOverlappingPair = findOverlapping instructionsByID unchecked
      (a,b) = fromMaybe freshPair mOverlappingPair
        where
          freshPair = head $ filter (isNotFound instructionsByID) unchecked
      instructionsForA = fst . head $ filter (\ el -> snd el == a) instructionsByID
      instr = (instructionToTranslateSecond (map (translateCoord instructionsForA) $ input !! a) (input !! b),b)
      remainingUnchecked = removeGivenElement (a,b) unchecked
  in (instr : instructionsByID, remainingUnchecked)

giveTranslated :: [(Instruction,Int)] -> [(Int,Int)] -> [[Coord]] -> [[Coord]]
giveTranslated instructionsByID unchecked input =
  if instructionsCoverAll instructionsByID unchecked
    then translateAll instructionsByID input
    else let (newInstructions, newUnchecked) = moreInstructions instructionsByID unchecked input
         in giveTranslated newInstructions newUnchecked input

-- idInstructions :: [(Instruction,Int)]
-- idInstructions = [(((P 'x', P 'y'),(0,0,0)),0)]

idInstructions :: [(Instruction,Int)]
idInstructions = [(((P 'x', P 'y', P 'z'),(0,0,0)),0)]

convertToMatchFirst :: [[Coord]] -> [[Coord]]
convertToMatchFirst input = giveTranslated idInstructions (allScannerPairs input) input

countAllBeacons :: [[Coord]] -> Int
countAllBeacons input =
  let allConverted = convertToMatchFirst input
  in length $ foldl1 union allConverted

firstPart :: IO Int
firstPart = countAllBeacons <$> input

giveAllScannerLocations :: [(Instruction,Int)] -> [(Int,Int)] -> [[Coord]] -> [Coord]
giveAllScannerLocations instructionsByID unchecked input =
  if instructionsCoverAll instructionsByID unchecked
    then map (snd . fst) instructionsByID
    else let (newInstructions, newUnchecked) = moreInstructions instructionsByID unchecked input
         in giveAllScannerLocations newInstructions newUnchecked input

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance a b =
  let (dx,dy,dz) = coordMinus a b
      in abs dx + abs dy + abs dz

maxManhattanDistance :: [Coord] -> Int
maxManhattanDistance = maximum . map (uncurry manhattanDistance) . allDifferentPairs

maxManhattanFromInput :: [[Coord]] -> Int
maxManhattanFromInput input = maxManhattanDistance $ giveAllScannerLocations idInstructions (allScannerPairs input) input

secondPart :: IO Int
secondPart = maxManhattanFromInput <$> input

testInputRaw :: [String]
testInputRaw = ["404,-588,-901"
            ,"528,-643,409"
            ,"-838,591,734"
            ,"390,-675,-793"
            ,"-537,-823,-458"
            ,"-485,-357,347"
            ,"-345,-311,381"
            ,"-661,-816,-575"
            ,"-876,649,763"
            ,"-618,-824,-621"
            ,"553,345,-567"
            ,"474,580,667"
            ,"-447,-329,318"
            ,"-584,868,-557"
            ,"544,-627,-890"
            ,"564,392,-477"
            ,"455,729,728"
            ,"-892,524,684"
            ,"-689,845,-530"
            ,"423,-701,434"
            ,"7,-33,-71"
            ,"630,319,-379"
            ,"443,580,662"
            ,"-789,900,-551"
            ,"459,-707,401"
            ,""
            ,"686,422,578"
            ,"605,423,415"
            ,"515,917,-361"
            ,"-336,658,858"
            ,"95,138,22"
            ,"-476,619,847"
            ,"-340,-569,-846"
            ,"567,-361,727"
            ,"-460,603,-452"
            ,"669,-402,600"
            ,"729,430,532"
            ,"-500,-761,534"
            ,"-322,571,750"
            ,"-466,-666,-811"
            ,"-429,-592,574"
            ,"-355,545,-477"
            ,"703,-491,-529"
            ,"-328,-685,520"
            ,"413,935,-424"
            ,"-391,539,-444"
            ,"586,-435,557"
            ,"-364,-763,-893"
            ,"807,-499,-711"
            ,"755,-354,-619"
            ,"553,889,-390"
            ,""
            ,"649,640,665"
            ,"682,-795,504"
            ,"-784,533,-524"
            ,"-644,584,-595"
            ,"-588,-843,648"
            ,"-30,6,44"
            ,"-674,560,763"
            ,"500,723,-460"
            ,"609,671,-379"
            ,"-555,-800,653"
            ,"-675,-892,-343"
            ,"697,-426,-610"
            ,"578,704,681"
            ,"493,664,-388"
            ,"-671,-858,530"
            ,"-667,343,800"
            ,"571,-461,-707"
            ,"-138,-166,112"
            ,"-889,563,-600"
            ,"646,-828,498"
            ,"640,759,510"
            ,"-630,509,768"
            ,"-681,-892,-333"
            ,"673,-379,-804"
            ,"-742,-814,-386"
            ,"577,-820,562"
            ,""
            ,"-589,542,597"
            ,"605,-692,669"
            ,"-500,565,-823"
            ,"-660,373,557"
            ,"-458,-679,-417"
            ,"-488,449,543"
            ,"-626,468,-788"
            ,"338,-750,-386"
            ,"528,-832,-391"
            ,"562,-778,733"
            ,"-938,-730,414"
            ,"543,643,-506"
            ,"-524,371,-870"
            ,"407,773,750"
            ,"-104,29,83"
            ,"378,-903,-323"
            ,"-778,-728,485"
            ,"426,699,580"
            ,"-438,-605,-362"
            ,"-469,-447,-387"
            ,"509,732,623"
            ,"647,635,-688"
            ,"-868,-804,481"
            ,"614,-800,639"
            ,"595,780,-596"
            ,""
            ,"727,592,562"
            ,"-293,-554,779"
            ,"441,611,-461"
            ,"-714,465,-776"
            ,"-743,427,-804"
            ,"-660,-479,-426"
            ,"832,-632,460"
            ,"927,-485,-438"
            ,"408,393,-506"
            ,"466,436,-512"
            ,"110,16,151"
            ,"-258,-428,682"
            ,"-393,719,612"
            ,"-211,-452,876"
            ,"808,-476,-593"
            ,"-575,615,604"
            ,"-485,667,467"
            ,"-680,325,-822"
            ,"-627,-443,-432"
            ,"872,-547,-609"
            ,"833,512,582"
            ,"807,604,487"
            ,"839,-516,451"
            ,"891,-625,532"
            ,"-652,-548,-490"
            ,"30,-46,-14"]

testInput :: [[Coord]]
testInput = map (map readCoord) . splitOn [""] $ testInputRaw

fourAndOneSeeRaw :: [String]
fourAndOneSeeRaw = ["459,-707,401"
                ,"-739,-1745,668"
                ,"-485,-357,347"
                ,"432,-2009,850"
                ,"528,-643,409"
                ,"423,-701,434"
                ,"-345,-311,381"
                ,"408,-1815,803"
                ,"534,-1912,768"
                ,"-687,-1600,576"
                ,"-447,-329,318"
                ,"-635,-1737,486"]

fourAndOneSee :: [Coord]
fourAndOneSee = map readCoord fourAndOneSeeRaw
