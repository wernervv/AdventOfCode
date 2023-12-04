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

getPossibleGears :: [String] -> [(Int,Int)]
getPossibleGears = concatMap (go 0) . zip [0..]
  where
    go _ (_,[]) = []
    go i (row, (c:cs)) =
      if (c == '*')
        then (row,i) : go (i+1) (row, cs)
        else go (i+1) (row, cs)

isAdjacentToNumberOneLine :: Int -> (Int,Int) -> Bool
isAdjacentToNumberOneLine x (start, end) = any (\ n -> x - n == 1 || x == n || n - x == 1) [start .. end]

getAdjacentNumberPosOneLine :: String -> Int -> [(Int,Int)]
getAdjacentNumberPosOneLine line x = filter (isAdjacentToNumberOneLine x) $ getNumberPositionsInOneLine line

getAllAdjacentNumberPos :: [String] -> (Int,Int) -> [NumberLocation]
getAllAdjacentNumberPos allLines (row, x) =
  let relevantRows = [max 0 (row - 1) .. min (row + 1) (length allLines - 1)]
      getAdjNumLoc allLines row x = map (\ n -> (row, n)) (getAdjacentNumberPosOneLine (allLines !! row) x)
  in concatMap (\ row -> getAdjNumLoc allLines row x) relevantRows

getNumberValueFromAll :: [String] -> NumberLocation -> Int
getNumberValueFromAll allLines (row, p) = getNumberValue p (allLines !! row)

getGearRatio :: [String] -> [NumberLocation] -> Int
getGearRatio allLines = product . map (getNumberValueFromAll allLines)

secondPuzzle :: IO Int
secondPuzzle = do
  allLines <- input
  return $ sum . map (getGearRatio allLines) . filter (\ ns -> length ns == 2) . map (getAllAdjacentNumberPos allLines) $ getPossibleGears allLines

