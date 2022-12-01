main :: IO ()
main = do
    contents <- readFile "day_1_input.txt"
    print . timesWindowIncreases . map readInt . words $ contents

readInt :: String -> Int
readInt = read

timesIncreases :: [Int] -> Int
timesIncreases [] = 0
timesIncreases [_] = 0
timesIncreases (x:y:rest) = incr + timesIncreases (y:rest)
  where
    incr = if y > x then 1 else 0

timesWindowIncreases :: [Int] -> Int
timesWindowIncreases [] = 0
timesWindowIncreases [_] = 0
timesWindowIncreases [_,_] = 0
timesWindowIncreases [_,_,_] = 0
timesWindowIncreases (x:r1:r2:y:rest) = incr + timesWindowIncreases (r1:r2:y:rest)
  where
    incr = if y > x then 1 else 0
