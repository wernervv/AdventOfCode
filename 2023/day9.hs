input :: IO [String]
input = lines <$> readFile "day9_input.txt"

readHistory :: String -> [Int]
readHistory = map read . words

giveDifferences :: [Int] -> [Int]
giveDifferences [] = []
giveDifferences [x] = []
giveDifferences (x:y:xs) = y - x : giveDifferences (y:xs)

allZeros :: [Int] -> Bool
allZeros = all (== 0)

extrapolate :: [Int] -> Int
extrapolate = go 0
  where
    go accum ns =
      if (allZeros ns)
        then accum
        else go (accum + last ns) (giveDifferences ns)

firstPuzzle :: IO Int
firstPuzzle = sum . map (extrapolate . readHistory) <$> input

extrapolatePrevious :: [Int] -> Int
extrapolatePrevious ns = go [] ns
  where
    go accum ns =
      if (allZeros ns)
        then foldl (flip (-)) 0 accum
        else go (head ns : accum) (giveDifferences ns)

secondPuzzle :: IO Int
secondPuzzle = sum . map (extrapolatePrevious . readHistory) <$> input

testInput :: [String]
testInput = ["0 3 6 9 12 15",
  "1 3 6 10 15 21",
  "10 13 16 21 30 45"]

