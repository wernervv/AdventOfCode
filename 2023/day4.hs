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

copiesProduced :: String -> Int
copiesProduced s =
  let ws = winningNumbers s
      ys = yourNumbers s
  in length . filter (isWinningNumber ws) $ ys

addCopiesToFollowing :: Int -> Int -> [Int] -> [Int]
addCopiesToFollowing extent multiplier following = zipWith (+) (take extent (repeat multiplier) ++ (repeat 0)) following

effectToFollowing :: [String] -> Int -> Int -> [Int] -> [Int]
effectToFollowing allCards cardNumber = addCopiesToFollowing (copiesProduced $ allCards !! cardNumber)

produceCopies :: [String] -> [Int]
produceCopies allCards =
  let amounts = take (length allCards) $ repeat 1
  in go 0 amounts allCards
    where
      go i [] allCards = []
      go i (x:xs) allCards = x : go (i+1) (effectToFollowing allCards i x xs) allCards

secondPuzzle :: IO Int
secondPuzzle = sum . produceCopies <$> input

testInput :: [String]
testInput = ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]

