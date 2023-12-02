import Data.Char (isDigit)

input :: IO [String]
input = lines <$> readFile "day1_input.txt"

pluckDigits :: String -> [Int]
pluckDigits = map (\ x -> (read :: String -> Int) [x]) . filter isDigit

combineFirstAndLast :: [Int] -> Int
combineFirstAndLast l = 10 * fi + la
  where
    fi = head l
    la = last l

firstPuzzle :: IO Int
firstPuzzle = sum <$> map (combineFirstAndLast . pluckDigits) <$> input
