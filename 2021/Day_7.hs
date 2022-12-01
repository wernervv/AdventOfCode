import Data.List.Split

input :: IO [Int]
input = map (read :: String -> Int) . splitOn "," <$> readFile "day_7_input.txt"

guess :: [Int] -> Int
guess input = (maximum input + minimum input) `div` 2

tweak :: Int -> Int -> (Int -> [Int] -> Int) -> [Int] -> Int
tweak value currentCost f positions
  | leftShiftCost < currentCost = tweak (value - 1) leftShiftCost f positions
  | rightShiftCost < currentCost = tweak (value + 1) rightShiftCost f positions
  | otherwise = currentCost
    where
      leftShiftCost = f (value - 1) positions
      rightShiftCost = f (value + 1) positions

cost :: Int -> [Int] -> Int
cost value = sum . map (abs . (value -))

firstPart :: IO Int
firstPart =
  do nums <- input
     let initialValue = guess nums
     return $ tweak initialValue (cost initialValue nums) cost nums

cost2 :: Int -> [Int] -> Int
cost2 value = sum . map (spanCost value)

spanCost :: Int -> Int -> Int
spanCost from to = sum [1..(abs (to - from))]

secondPart :: IO Int
secondPart =
  do nums <- input
     let initialValue = guess nums
     return $ tweak initialValue (cost2 initialValue nums) cost2 nums

testInput :: [Int]
testInput = [16,1,2,0,4,2,7,1,2,14]
