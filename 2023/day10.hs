input :: IO [String]
input = lines <$> readFile "day10_input.txt"

type Grid = [[Char]]

type Coordinates = (Int, Int)

data Direction = N | E | S | W deriving (Eq)

giveStartRow :: Grid -> Int
giveStartRow = go 0
  where
    go n [] = -1
    go n (row:rest) = if ('S' `elem` row) then n else go (n+1) rest

giveStartIndexInRow :: Int -> Grid -> Int
giveStartIndexInRow row g = go 0 (g !! row)
  where
    go n [] = -1
    go n (c:cs) = if (c == 'S') then n else go (n+1) cs

giveStart :: Grid -> Coordinates
giveStart g = (x,y)
  where
    y = giveStartRow g
    x = giveStartIndexInRow y g

giveOtherVal :: Eq a => a -> (a,a) -> Maybe a
giveOtherVal c (a,b) =
  if (c == a)
    then Just b
    else if (c == b)
           then Just a
           else Nothing

allPipes :: [(Char, (Direction, Direction))]
allPipes = [('|', (N,S)),
  ('-', (W,E)),
  ('L', (N,E)),
  ('J', (N,W)),
  ('7', (W,S)),
  ('F', (E,S))]

followPipe :: Char -> Direction -> Maybe Direction
followPipe 'S' dir = Just dir
followPipe c dir = giveOtherVal dir . snd . head . filter ((== c) . fst) $ allPipes

giveCoordinatesInDirection :: Grid -> Coordinates -> Direction -> Maybe Coordinates
giveCoordinatesInDirection g (x,y) dir =
  let maxX = length (g !! 0) - 1
      maxY = length g - 1
      newX = case dir of
        E -> x + 1
        W -> x - 1
        _ -> x
      newY = case dir of
        N -> y - 1
        S -> y + 1
        _ -> y
  in if (newX < 0 || newX > maxX || newY < 0 || newY > maxY) then Nothing else Just (newX, newY)

flipDirection :: Direction -> Direction
flipDirection N = S
flipDirection S = N
flipDirection W = E
flipDirection E = W

moveOneStep :: Grid -> (Coordinates,Direction) -> Maybe (Coordinates,Direction)
moveOneStep g (coords, arrivingDir) = do
  leavingDir <- followPipe (getTile g coords) arrivingDir
  nextCoords <- giveCoordinatesInDirection g coords leavingDir
  return (nextCoords, flipDirection leavingDir)

loopsInDirection :: Grid -> Coordinates -> Direction -> Direction -> Bool
loopsInDirection g coords initialDir arrivingDir =
  if (coords == (giveStart g) && arrivingDir /= initialDir)
    then True
    else case moveOneStep g (coords, arrivingDir) of
      Nothing -> False
      Just (newCoords, newArrivingDir) -> loopsInDirection g newCoords initialDir newArrivingDir

getTile :: Grid -> Coordinates -> Char
getTile g (x,y) = g !! y !! x

countLoopLength :: Grid -> Coordinates -> Direction -> Int
countLoopLength = go 0
  where
    go n g coords arrivingDir =
      if (coords == (giveStart g) && n > 0)
        then n + 1
        else let (newCoords, newArrivingDir) = (\ (Just p) -> p) $ moveOneStep g (coords, arrivingDir)
             in go (n+1) g newCoords newArrivingDir

farthestFromStart :: Grid -> Int
farthestFromStart g = loopLength `div` 2
  where
    loopLength = countLoopLength g startCoords loopDir
    startCoords = giveStart g
    loopDir = head . filter (\ dir -> loopsInDirection g startCoords dir dir) $ [N,E,S,W]

firstPuzzle :: IO Int
firstPuzzle = farthestFromStart <$> input

coordinatesFromStart :: Grid -> Coordinates -> Direction -> [Coordinates]
coordinatesFromStart g startCoords dir = go [] g startCoords dir
  where
    go accum g currentCoords arrivingDir =
      if (currentCoords == startCoords && length accum > 0)
        then accum
        else let (newCoords, newArrivingDir) = (\ (Just p) -> p) $ moveOneStep g (currentCoords, arrivingDir)
             in go (currentCoords : accum) g newCoords newArrivingDir

giveLoopCoordinates :: Grid -> [Coordinates]
giveLoopCoordinates g = coordinatesFromStart g startCoords loopDir
  where
    startCoords = giveStart g
    loopDir = head . filter (\ dir -> loopsInDirection g startCoords dir dir) $ [N,E,S,W]

isPartOfLoop :: [Coordinates] -> Coordinates -> Bool
isPartOfLoop = flip elem

areOppositeChars :: Char -> Char -> Bool
areOppositeChars 'F' 'J' = True
areOppositeChars 'L' '7' = True
areOppositeChars 'J' 'F' = True
areOppositeChars '7' 'L' = True
areOppositeChars _ _ = False

countSection :: Grid -> [Coordinates] -> (Int, [Coordinates])
countSection g (c:cs) =
  let startChar = getTile g c
      skippedUntilRelevant = dropWhile (\ c -> getTile g c == '-') cs
      endChar = getTile g (head skippedUntilRelevant)
      count = if (areOppositeChars startChar endChar)
                then 1
                else 0
  in (count, tail skippedUntilRelevant)

countDistinctFromCoordinates :: Grid -> [Coordinates] -> [Coordinates] -> Int -> Int
countDistinctFromCoordinates _ _ [] n = n
countDistinctFromCoordinates g loopCoords coords@(c:cs) n =
  if (isPartOfLoop loopCoords c)
    then if (getTile g c == '|')
           then countDistinctFromCoordinates g loopCoords cs (n+1) 
           else let (count, remainingCoords) = countSection g coords
                in countDistinctFromCoordinates g loopCoords remainingCoords (n+count)
    else countDistinctFromCoordinates g loopCoords cs n

countDistinctLoopPartsToEdge :: Grid -> [Coordinates] -> Coordinates -> Int
countDistinctLoopPartsToEdge g loopCoords (x,y) =
  let maxX = length (g !! 0) - 1
      (startX, startY) = giveStart g
      startIsToTheRight = y == startY && startX > x
      allUntilEdge =
        if (startIsToTheRight)
          then reverse $ map (\ x -> (x,y)) [0..(x-1)]
          else map (\ x -> (x,y)) [(x+1)..maxX]
  in countDistinctFromCoordinates g loopCoords allUntilEdge 0

isInsideLoop :: Grid -> [Coordinates] -> Coordinates -> Bool
isInsideLoop g loopCoords c = odd $ countDistinctLoopPartsToEdge g loopCoords c

countAllInsideLoop :: Grid -> Int
countAllInsideLoop g =
  let maxX = length (g !! 0) - 1
      maxY = length g - 1
      allCoords = [(x,y) | y <- [0..maxY], x <- [0..maxX]]
      loopCoords = giveLoopCoordinates g
  in length . filter (\ c -> not (isPartOfLoop loopCoords c) && isInsideLoop g loopCoords c) $ allCoords

secondPuzzle :: IO Int
secondPuzzle = countAllInsideLoop <$> input

testInput :: [String]
testInput = ["-L|F7",
  "7S-7|",
  "L|7||",
  "-L-J|",
  "L|-JF"]

biggerTest :: [String]
biggerTest = ["FF7FSF7F7F7F7F7F---7",
  "L|LJ||||||||||||F--J",
  "FL-7LJLJ||||||LJL-77",
  "F--JF--7||LJLJ7F7FJ-",
  "L---JF-JLJ.||-FJLJJ7",
  "|F|F-JF---7F7-L7L|7|",
  "|FFJF7L7F-JF7|JL---7",
  "7-L-JL7||F7|L7F-7F7|",
  "L.L7LFJ|||||FJL7||LJ",
  "L7JLJL-JLJLJL--JLJ.L"]

