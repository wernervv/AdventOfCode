import Data.Char (isDigit)
import Control.Applicative

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

spelledDigits :: [String]
spelledDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

parseChar :: Char -> String -> Maybe String
parseChar _ [] = Nothing
parseChar c (x:xs) = if (c == x) then Just xs else Nothing

parseString :: String -> String -> Maybe String
parseString [] s = Just s
parseString (c:cs) s = parseChar c s >>= parseString cs 

parseSpelled :: String -> Int -> String -> Maybe Int
parseSpelled word n s = parseString word s >>= \ _ -> Just n

digitFromBeginning :: String -> Maybe Int
digitFromBeginning [] = Nothing
digitFromBeginning s@(c:cs) =
  if (isDigit c)
    then Just (read [c])
    else foldl1 (<|>) $ map ($ s) $ zipWith parseSpelled spelledDigits [1..]

parseFirstDigit :: String -> Int
parseFirstDigit [] = 0
parseFirstDigit s@(_:cs) = case digitFromBeginning s of
  Just n -> n
  Nothing -> parseFirstDigit cs

digitFromEnd :: String -> Maybe Int -- operates on reversed strings
digitFromEnd [] = Nothing
digitFromEnd s@(c:cs) =
  if (isDigit c)
    then Just (read [c])
    else foldl1 (<|>) $ map ($ s) $ zipWith parseSpelled (map reverse spelledDigits) [1..]

parseLastDigit :: String -> Int
parseLastDigit [] = 0
parseLastDigit s = go $ reverse s
  where
    go s@(_:cs) = case digitFromEnd s of
      Just n -> n
      Nothing -> go cs

getCalibrationValue :: String -> Int
getCalibrationValue s = 10 * (parseFirstDigit s) + (parseLastDigit s)

secondPuzzle :: IO Int
secondPuzzle = sum . map getCalibrationValue <$> input

