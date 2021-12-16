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
