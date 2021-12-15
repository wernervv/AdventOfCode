import Data.List.Split

type Notes = [([String], [String])] 

input :: IO Notes
input = map ((\ [x,y] -> (splitOn " " x, splitOn " " y)) . splitOn " | ") . lines <$> readFile "day_8_input.txt"

allOutput :: Notes -> [[String]]
allOutput = map snd

isOne :: String -> Bool
isOne = (== 2) . length

isFour :: String -> Bool
isFour = (== 4) . length

isSeven :: String -> Bool
isSeven = (== 3) . length

isEight :: String -> Bool
isEight = (== 7) . length

isEasyDigit :: String -> Bool
isEasyDigit str = isOne str || isFour str || isSeven str || isEight str

firstPart :: IO Int
firstPart = length . filter isEasyDigit . concat . allOutput <$> input
