import Data.Char (isDigit)

input :: IO [String]
input = lines <$> readFile "day3_input.txt"

type SymbolLocations = [[Int]]

type NumberLocation = (Int, (Int, Int))

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

getSymbolsInOneLine :: String -> [Int]
getSymbolsInOneLine = map fst . filter snd . zip [0..] . map isSymbol

getSymbolPositions :: [String] -> SymbolLocations
getSymbolPositions = map getSymbolsInOneLine

groupPositionsInOneLine :: [Int] -> [(Int, Int)]
groupPositionsInOneLine [] = []
groupPositionsInOneLine (n:ns) = go [] n n ns
  where
    go allNums start current [] = (start,current) : allNums
    go allNums start current (n:ns) =
      if (n == current + 1)
        then go allNums start n ns
        else go ((start, current) : allNums) n n ns

getNumberPositionsInOneLine :: String -> [(Int, Int)]
getNumberPositionsInOneLine = groupPositionsInOneLine . map fst . filter snd . zip [0..] . map isDigit

isAdjacentOneRow :: (Int, Int) -> [Int] -> Bool
isAdjacentOneRow (start, end) syms = or $ map (\ n -> any (\ x -> n == x || n - x == 1 || x - n == 1) syms) [start .. end]

isAdjacentToSymbol :: NumberLocation -> SymbolLocations -> Bool
isAdjacentToSymbol (row, pos) allSyms =
  let relevantRows = [max 0 (row - 1) .. min (length allSyms - 1) (row + 1)]
  in any (isAdjacentOneRow pos) (map (allSyms !!) relevantRows)

getNumberValue :: (Int,Int) -> String -> Int
getNumberValue (start, end) s = read $ map (s !!) [start .. end]

partNumbersInOneLine :: SymbolLocations -> Int -> String -> [Int]
partNumbersInOneLine allSyms row s =
  let numberPosInLine = getNumberPositionsInOneLine s
      relevantNumberPos = filter (\ pos -> isAdjacentToSymbol (row, pos) allSyms) numberPosInLine
  in map (\ pos -> getNumberValue pos s) relevantNumberPos

firstPuzzle :: IO Int
firstPuzzle = do
  allLines <- input
  let syms = getSymbolPositions allLines
  return $ sum . map (\ (line, row) -> sum $ partNumbersInOneLine syms row line) $ zip allLines [0..]
