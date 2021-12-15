import Data.List.Split

input :: IO [Int]
input = map (read :: String -> Int) . splitOn "," <$> readFile "day_6_input.txt"

initInput :: Int -> [Int] -> [(Int, Int)]
initInput days xs = zip xs (repeat days)

day :: [Int] -> [Int]
day [] = []
day (x:xs) =
  if x == 0
    then 6 : day xs ++ [8]
    else (x-1) : day xs

firstPart :: IO Int
firstPart = sum . map allSubsequentCount . initInput 80 <$> input

allSubsequentCount :: (Int, Int) -> Int
allSubsequentCount (ctr, daysLeft) =
  if daysLeft - ctr > 0
    then 1 + sum (map allSubsequentCount subsequent)
    else 1
      where
        subsequent = giveSubsequent offset daysLeft
        offset = ctr + 1

giveSubsequent :: Int -> Int -> [(Int, Int)]
giveSubsequent offset daysLeft =
  if daysLeft >= offset
    then (8, daysLeft - offset) : giveSubsequent 7 (daysLeft - offset)
    else []

testInput :: [Int]
testInput = [3,4,3,1,2]
