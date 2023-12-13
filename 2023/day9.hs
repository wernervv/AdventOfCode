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

