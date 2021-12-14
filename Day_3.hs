type Count = (Int, Int) -- (0s, 1s)

input :: IO [String]
input = lines <$> readFile "day_3_input.txt"

countsForAll :: [String] -> [Count]
countsForAll input = foldl (zipWith countBit) (replicate n (0,0)) input
  where
    n = length $ head input

countBit :: Count -> Char -> Count
countBit (zeros, ones) c = case c of
  '0' -> (zeros + 1, ones)
  '1' -> (zeros, ones + 1)
  _   -> (zeros, ones)

mostCommon :: Count -> Int
mostCommon (z,o) = if z > o then 0 else 1

leastCommon :: Count -> Int
leastCommon (z,o) = if z < o then 0 else 1

gamma :: [Count] -> [Int]
gamma = map mostCommon

epsilon :: [Count] -> [Int]
epsilon = map leastCommon

binToDec :: [Int] -> Int
binToDec input = helper (reverse input) 0 1
  where
    helper [] acc multiplier = acc
    helper (x:xs) acc multiplier = helper xs (acc + x * multiplier) (2 * multiplier)

firstPart :: IO Int
firstPart = (*) <$> gammaRate <*> epsilonRate
  where
    gammaRate = binToDec . gamma . countsForAll <$> input
    epsilonRate = binToDec . epsilon . countsForAll <$> input
