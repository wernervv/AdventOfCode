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

testInput :: [String]
testInput = ["-L|F7",
  "7S-7|",
  "L|7||",
  "-L-J|",
  "L|-JF"]

