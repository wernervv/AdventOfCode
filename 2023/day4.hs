input :: IO [String]
input = lines <$> readFile "day4_input.txt"

winningNumbers :: String -> [Int]
winningNumbers = map read . words . takeWhile (/= '|') . drop 1 . dropWhile (/= ':')

yourNumbers :: String -> [Int]
yourNumbers = map read . words . drop 1 . dropWhile (/= '|')

isWinningNumber :: [Int] -> Int -> Bool
isWinningNumber ws n = n `elem` ws

pointsFromMatched :: [Int] -> Int
pointsFromMatched [] = 0
pointsFromMatched l = 2 ^ (length l - 1)

pointsPerCard :: String -> Int
pointsPerCard s =
  let ws = winningNumbers s
      ys = yourNumbers s
  in pointsFromMatched . filter (isWinningNumber ws) $ ys

firstPuzzle :: IO Int
firstPuzzle = sum . map pointsPerCard <$> input

