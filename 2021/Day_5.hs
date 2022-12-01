import Data.List.Split
import Data.Bifunctor(bimap)
import Test.QuickCheck (arbitrarySizedFractional)

type Point = (Int, Int)
type Line = (Point, Point)

input :: IO [String]
input = lines <$> readFile "day_5_input.txt"

lineInput :: IO [Line]
lineInput = map makeToLine <$> input
  where
    makeToLine = bimap separateCoords separateCoords . beginEnd

beginEnd :: String -> (String, String)
beginEnd = (\ xs -> (xs !! 0, xs !! 1)) . splitOn "->"

separateCoords :: String -> (Int, Int)
separateCoords = (\ xs -> (xs !! 0, xs !! 1)) . map (read :: String -> Int) . splitOn ","

allXCoords :: IO [Int]
allXCoords = concatMap lineXCoords <$> lineInput

lineXCoords :: Line -> [Int]
lineXCoords ((x1,_),(x2,_)) = [x1,x2]

allYCoords :: IO [Int]
allYCoords = concatMap lineYCoords <$> lineInput

lineYCoords :: Line -> [Int]
lineYCoords ((_,y1),(_,y2)) = [y1,y2]

maxSize :: IO Point
maxSize = (,) <$> ((+1) . maximum <$> allXCoords) <*> ((+1) . maximum <$> allYCoords)

initField :: IO [[Int]]
initField = (\ (x,y) -> replicate y $ replicate x 0) <$> maxSize

isHorizontal :: Line -> Bool
isHorizontal = (\ [a,b] -> a == b) . lineYCoords

isVertical :: Line -> Bool
isVertical = (\ [a,b] -> a == b) . lineXCoords

isDiagonal :: Line -> Bool
isDiagonal ((x1,y1), (x2,y2)) = abs(x2 - x1) == abs(y2 - y1)

generateLine :: Line -> [Point]
generateLine line@((x1,y1),(x2,y2))
    | isHorizontal line = zip (numbersInRange x1 x2) (repeat y1)
    | isVertical line = zip (repeat x1) (numbersInRange y1 y2)
    | isDiagonal line = zip (numbersInRange x1 x2) (numbersInRange y1 y2)
    | otherwise = []

numbersInRange :: Int -> Int -> [Int]
numbersInRange x y = if x > y then reverse [y..x] else [x..y]

updateOne :: Point -> [[Int]] -> [[Int]]
updateOne (x,y) = helper y x []
  where
    helper y x acc [] = acc
    helper y x acc (l:ls) =
      if y == 0
        then acc ++ [updateX x l] ++ ls
        else acc ++ [l] ++ helper (y-1) x acc ls
          where
            updateX _ [] = []
            updateX x (y:ys) =
              if x == 0
                then (y+1):ys
                else y : updateX (x-1) ys

drawLine :: Line -> [[Int]] -> [[Int]]
drawLine line = if isVertical line || isHorizontal line || isDiagonal line
    then flip (foldl (flip ($))) (map updateOne $ generateLine line)
    else id

allOverlaps :: [[Int]] -> Int
allOverlaps = sum . map (length . filter (> 1))

allPoints :: [Line] -> [Point]
allPoints = concatMap generateLine

overlappingCount :: [Point] -> Int
overlappingCount [] = 0
overlappingCount (p:ps) = thisOverlaps + overlappingCount modified
  where
    thisOverlaps = if p `elem` ps then 1 else 0
    modified = filter (/= p) ps

firstAndSecondPart :: IO Int
firstAndSecondPart = allOverlaps <$> (foldl (flip ($)) <$> initField <*> (map drawLine <$> lineInput))

firstPart2 :: IO Int
firstPart2 = overlappingCount . allPoints <$> lineInput

testInput :: [String]
testInput = ["0,9 -> 5,9"
            ,"8,0 -> 0,8"
            ,"9,4 -> 3,4"
            ,"2,2 -> 2,1"
            ,"7,0 -> 7,4"
            ,"6,4 -> 2,0"
            ,"0,9 -> 2,9"
            ,"3,4 -> 1,4"
            ,"0,0 -> 8,8"
            ,"5,5 -> 8,2"]

testLine :: [Line]
testLine = map makeToLine testInput
  where
    makeToLine = bimap separateCoords separateCoords . beginEnd

testField :: [[Int]]
testField = replicate 10 $ replicate 10 0
