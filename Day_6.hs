import Data.List.Split

type CycleState = (Int,Int,Int,Int,Int,Int,Int,Int,Int)

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

day2 :: CycleState -> CycleState
day2 (ze,on,tw,th,fo,fi,si,se,ei) = (on, tw, th, fo, fi, si, se + ze, ei, ze)

initCycleState :: [Int] -> CycleState
initCycleState input = (f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8)
  where
    f n = length $ filter (== n) input

allBasedOnCycleState :: CycleState -> Int
allBasedOnCycleState (ze,on,tw,th,fo,fi,si,se,ei) = ze + on + tw + th + fo + fi + si + se + ei

secondPart :: IO Int
secondPart = allBasedOnCycleState . flip (foldl (flip ($))) (replicate 256 day2) . initCycleState <$> input

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
