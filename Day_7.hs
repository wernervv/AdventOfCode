import Data.List.Split

input :: IO [Int]
input = map (read :: String -> Int) . splitOn "," <$> readFile "day_7_input.txt"

guess :: [Int] -> Int
guess input = (maximum input + minimum input) `div` 2

tweak :: Int -> Int -> [Int] -> Int
tweak value currentCost positions
  | leftShiftCost < currentCost = tweak (value - 1) leftShiftCost positions
  | rightShiftCost < currentCost = tweak (value + 1) rightShiftCost positions
  | otherwise = currentCost
    where
      leftShiftCost = cost (value - 1) positions
      rightShiftCost = cost (value + 1) positions

cost :: Int -> [Int] -> Int
cost value = sum . map (abs . (value -))

firstPart :: IO Int
firstPart =
  do nums <- input
     let initialValue = guess nums
     return $ tweak initialValue (cost initialValue nums) nums
