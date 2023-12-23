input :: IO [String]
input = lines <$> readFile "day14_input.txt"

type Grid = [[Char]]

type Line = [Char]

giveRow :: Grid -> Int -> Line
giveRow = (!!)

giveColumn :: Grid -> Int -> Line
giveColumn g n =
  let biggestY = length g - 1
  in map (\ y -> g !! y !! n) [0..biggestY]

giveCubicIndeces :: Line -> [Int]
giveCubicIndeces = go 0
  where
    go _ [] = []
    go n ('#':cs) = n : go (n+1) cs
    go n (c:cs) = go (n+1) cs

giveRoundBetweenIndeces :: Line -> Int -> Int -> Int
giveRoundBetweenIndeces l s e = length . filter (\ n -> l !! n == 'O') $ [s+1 .. e-1]

giveAllRounds :: [Int] -> Line -> [Int]
giveAllRounds [_] l = []
giveAllRounds (x:y:xs) l = giveRoundBetweenIndeces l x y : giveAllRounds (y:xs) l

giveOneSetOfRounds :: Int -> Int -> (Line,Int)
giveOneSetOfRounds n ind = (take n $ repeat 'O', ind+n)

fillUntilNextCubic :: Line -> Int -> Int -> Line
fillUntilNextCubic line ind n = line ++ (take (n - ind) $ repeat '.')

constructLine :: [Int] -> [Int] -> Int -> Line
constructLine cubics rounds lastIndex =
  let (lineStart, ind) = giveOneSetOfRounds (head rounds) 0
  in go lineStart ind cubics (tail rounds) lastIndex
    where
      go lineSoFar currentIndex [] [] lastIndex = lineSoFar ++ (take (lastIndex - currentIndex + 1) $ repeat '.')
      go lineSoFar currentIndex (c:cubics) (r:rounds) lastIndex =
        let (nextSetOfRounds, newInd) = giveOneSetOfRounds r (c+1)
        in go (fillUntilNextCubic lineSoFar currentIndex c ++ '#' : nextSetOfRounds) newInd cubics rounds lastIndex

giveLineAfterRolling :: Line -> Line
giveLineAfterRolling l =
  let lastIndex = length l - 1
      cubics = giveCubicIndeces l
      roundsPerInterval = giveAllRounds (-1 : cubics ++ [lastIndex+1]) l
  in constructLine cubics roundsPerInterval lastIndex

countLoad :: Line -> Int
countLoad l = helper 0 1 (reverse l)
  where
    helper load _ [] = load
    helper load n ('O':cs) = helper (load+n) (n+1) cs
    helper load n (c:cs) = helper load (n+1) cs

countTotalLoad :: Grid -> Int
countTotalLoad g =
  let biggestX = length (head g) - 1
  in sum . map (\ n -> countLoad . giveLineAfterRolling $ giveColumn g n) $ [0..biggestX]

firstPuzzle :: IO Int
firstPuzzle = countTotalLoad <$> input

testInput :: [String]
testInput = ["O....#....",
  "O.OO#....#",
  ".....##...",
  "OO.#O....O",
  ".O.....O#.",
  "O.#..O.#.#",
  "..O..#O..O",
  ".......O..",
  "#....###..",
  "#OO..#...."]
