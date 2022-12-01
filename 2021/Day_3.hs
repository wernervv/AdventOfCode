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
leastCommon (z,o) = if z > o then 1 else 0

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

oxygen :: [String] -> String
oxygen input = oxygenHelper input 0
  where
    oxygenHelper input pos =
      if length input == 1
        then head input
        else oxygenHelper (filter (\ str -> str !! pos == common) input) (pos+1)
            where
            count = foldl (\ count str -> countBit count (str !! pos)) (0,0) input
            common = case mostCommon count of
                        0 -> '0'
                        _ -> '1'

c02 :: [String] -> String
c02 input = c02Helper input 0
  where
    c02Helper input pos =
      if length input == 1
        then head input
        else c02Helper (filter (\ str -> str !! pos == uncommon) input) (pos+1)
            where
            count = foldl (\ count str -> countBit count (str !! pos)) (0,0) input
            uncommon = case leastCommon count of
                        1 -> '1'
                        _ -> '0'

secondPart :: IO Int
secondPart = (*) <$> oxygenRating <*> c02Rating
  where
    oxygenRating = binToDec . map ((read :: String -> Int) . (: [])) . oxygen <$> input
    c02Rating = binToDec . map ((read :: String -> Int) . (: [])) . c02 <$> input
