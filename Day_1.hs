main :: IO ()
main = do
    contents <- readFile "day_1_input.txt"
    print . timesIncreases . map readInt . words $ contents

readInt :: String -> Int
readInt = read

timesIncreases :: [Int] -> Int
timesIncreases [] = 0
timesIncreases [_] = 0
timesIncreases (x:y:rest) = incr + timesIncreases (y:rest)
  where
    incr = if y > x then 1 else 0
