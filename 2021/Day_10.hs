import Data.List(sort)

input :: IO [String]
input = lines <$> readFile "day_10_input.txt"

isCorrupted :: String -> Bool
isCorrupted = helper . removeValidChunks
  where
    helper [] = False
    helper s =  any isClosingBracket s

allCorrupted :: [String] -> [String]
allCorrupted = filter isCorrupted

firstIllegal :: String -> Char
firstIllegal = head . filter isClosingBracket

isOpeningBracket :: Char -> Bool
isOpeningBracket = (`elem` "([{<")

isClosingBracket :: Char -> Bool
isClosingBracket = (`elem` ")]}>")

isMatchingFor :: Char -> Char -> Bool
isMatchingFor open close =
  case open of
    '(' -> close == ')'
    '[' -> close == ']'
    '{' -> close == '}'
    '<' -> close == '>'
    _ -> False

removeValidChunks :: String -> String
removeValidChunks [] = []
removeValidChunks [x] = [x]
removeValidChunks orig@(x:y:rest)
  | isClosingBracket x = orig
  | isMatchingFor x y = removeValidChunks rest
  | null rest = [x,y]
  | subIsShorter = removeValidChunks $ x : sub
  | otherwise = x : sub
    where
      sub = removeValidChunks (y:rest)
      subIsShorter = sub `areDifferent` (y:rest)

areDifferent :: Eq a => [a] -> [a] -> Bool
areDifferent [] [] = False
areDifferent (_:_) [] = True
areDifferent [] (_:_) = True
areDifferent (x:xs) (y:ys) = (x /= y) || areDifferent xs ys

syntaxErrorScore :: Char -> Int
syntaxErrorScore c =
  case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> 0

firstPart :: IO Int
firstPart = sum . map (syntaxErrorScore . firstIllegal . removeValidChunks) . allCorrupted <$> input

allIncomplete :: [String] -> [String]
allIncomplete = filter (not . isCorrupted)

completesBy :: String -> String
completesBy = helper . reverse . removeValidChunks
  where
    helper [] = []
    helper (x:xs) = counterPart x : helper xs

counterPart :: Char -> Char
counterPart c =
  case c of
    '(' -> ')'
    '{' -> '}'
    '[' -> ']'
    '<' -> '>'
    _   -> '1'

completeCharScore :: Char -> Int
completeCharScore c =
  case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _   -> 0

countCompleteScore :: String -> Int
countCompleteScore = helper 0
  where
    helper acc [] = acc
    helper acc (x:xs) = helper newAcc xs
      where
        newAcc = 5*acc + completeCharScore x

middleVal :: [a] -> a
middleVal l = l !! ((length l - 1) `div` 2)

secondPart :: IO Int
secondPart = middleVal . sort . map (countCompleteScore . completesBy) . allIncomplete <$> input
