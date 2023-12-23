input :: IO [String]
input = lines <$> readFile "day13_input.txt"

type Pattern = [String]

readOnePattern :: [String] -> (Pattern, [String])
readOnePattern [] = ([],[])
readOnePattern ("":rest) = ([],rest)
readOnePattern (s:rest) = (s:remainingRelevant, restAfterAll)
  where
    (remainingRelevant, restAfterAll) = readOnePattern rest

readPatterns :: [String] -> [Pattern]
readPatterns [] = []
readPatterns ("":rest) = readPatterns rest
readPatterns s =
  let (read, rest) = readOnePattern s
  in read : readPatterns rest

giveRow :: Pattern -> Int -> String
giveRow = (!!)

giveColumn :: Pattern -> Int -> String
giveColumn p n =
  let biggestY = length p - 1
  in map (\ y -> p !! y !! n) [0..biggestY]

giveFirstReflectedRow :: Pattern -> Int -> Maybe Int
giveFirstReflectedRow p n =
  let biggestY = length p - 1
      helper n k =
        if (k > biggestY)
          then Nothing
          else if (giveRow p n == giveRow p k)
            then Just k
            else helper n (k+1)
  in helper n (n+1)

isPerfectReflectionRows :: Pattern -> Int -> Bool
isPerfectReflectionRows p aboveReflectionLine =
  let firstRows = reverse . take aboveReflectionLine $ p
      lastRows = drop aboveReflectionLine p
  in all (uncurry (==)) $ zip firstRows lastRows

giveRowsAboveReflectionLine :: Pattern -> Maybe Int
giveRowsAboveReflectionLine p =
  let biggestY = length p - 1
      go _ [] = Nothing
      go p (n:ns) = case giveFirstReflectedRow p n of
        (Just x) -> if (isPerfectReflectionRows p refl)
          then Just refl
          else go p ns
            where refl = (n + x) `div` 2 + 1
        Nothing -> go p ns
  in go p [0..biggestY]

giveFirstReflectedColumn :: Pattern -> Int -> Maybe Int
giveFirstReflectedColumn p n =
  let biggestX = length (head p) - 1
      helper n k =
        if (k > biggestX)
          then Nothing
          else if (giveColumn p n == giveColumn p k)
            then Just k
            else helper n (k+1)
  in helper n (n+1)

isPerfectReflectionColumns :: Pattern -> Int -> Bool
isPerfectReflectionColumns p leftOfReflectionLine =
  let biggestX = length (head p) - 1
      firstColumns = reverse . map (giveColumn p) $ [0..leftOfReflectionLine-1]
      lastColumns = map (giveColumn p) [leftOfReflectionLine..biggestX]
  in all (uncurry (==)) $ zip firstColumns lastColumns

giveColumnsLeftOfReflectionLine :: Pattern -> Maybe Int
giveColumnsLeftOfReflectionLine p =
  let biggestX = length (head p) - 1
      go _ [] = Nothing
      go p (n:ns) = case giveFirstReflectedColumn p n of
        (Just x) -> if (isPerfectReflectionColumns p refl)
          then Just refl
          else go p ns
            where refl = (n + x) `div` 2 + 1
        Nothing -> go p ns
  in go p [0..biggestX]

countSummary :: Pattern -> Int
countSummary p =
  case giveRowsAboveReflectionLine p of
    (Just rows) -> 100 * rows
    Nothing -> case giveColumnsLeftOfReflectionLine p of
      (Just columns) -> columns
      Nothing -> 0

firstPuzzle :: IO Int
firstPuzzle = sum . map countSummary . readPatterns <$> input

countDifferencesBetweenTwoLines :: String -> String -> Int
countDifferencesBetweenTwoLines a b = length . filter (uncurry (/=)) $ zip a b

countDifferencesBetweenSets :: [String] -> [String] -> Int
countDifferencesBetweenSets a b = sum . map (uncurry countDifferencesBetweenTwoLines) $ zip a b

isPerfectReflectionWithSmudgeRows :: Pattern -> Int -> Bool
isPerfectReflectionWithSmudgeRows p n =
  let biggestY = length p - 1
      firstRows = reverse $ take n p
      lastRows = drop n p
  in countDifferencesBetweenSets firstRows lastRows == 1

findNewReflectionRow :: Pattern -> Int -> Maybe Int
findNewReflectionRow p n =
  let biggestY = length p - 1
      rowsToCheck = filter (/= n) [1..biggestY]
      pickFirstMatching _ [] = Nothing
      pickFirstMatching p (n:ns) =
        if (isPerfectReflectionWithSmudgeRows p n)
          then Just n
          else pickFirstMatching p ns
  in pickFirstMatching p rowsToCheck 

isPerfectReflectionWithSmudgeColumns :: Pattern -> Int -> Bool
isPerfectReflectionWithSmudgeColumns p n =
  let biggestY = length p - 1
      biggestX = length (head p) - 1
      allColumns = map (\ x -> map (\ y -> p !! y !! x) [0..biggestY]) [0..biggestX]
      firstColumns = reverse $ take n allColumns
      lastColumns = drop n allColumns
  in countDifferencesBetweenSets firstColumns lastColumns == 1

findNewReflectionColumn :: Pattern -> Int -> Maybe Int
findNewReflectionColumn p n =
  let biggestX = length (head p) - 1
      columnsToCheck = filter (/= n) [1..biggestX]
      pickFirstMatching _ [] = Nothing
      pickFirstMatching p (n:ns) =
        if (isPerfectReflectionWithSmudgeColumns p n)
          then Just n
          else pickFirstMatching p ns
  in pickFirstMatching p columnsToCheck

giveReflectionLineRows :: Pattern -> Maybe Int
giveReflectionLineRows p =
  case giveRowsAboveReflectionLine p of
    (Just x) -> if (isPerfectReflectionRows p x)
      then Just x
      else Nothing
    Nothing -> Nothing

giveReflectionLineColumns :: Pattern -> Maybe Int
giveReflectionLineColumns p =
  case giveColumnsLeftOfReflectionLine p of
    (Just x) -> if (isPerfectReflectionColumns p x)
      then Just x
      else Nothing
    Nothing -> Nothing

countSummaryWithSmudges :: Pattern -> Int
countSummaryWithSmudges p =
  case giveReflectionLineRows p of
    (Just a) -> case findNewReflectionRow p a of
      (Just b) -> 100 * b
      Nothing -> case findNewReflectionColumn p (-1) of
        (Just c) -> c
        Nothing -> 0
    Nothing -> case giveReflectionLineColumns p of
      (Just d) -> case findNewReflectionColumn p d of
        (Just e) -> e
        Nothing -> case findNewReflectionRow p (-1) of
          (Just f) -> 100 * f
          Nothing -> 0

secondPuzzle :: IO Int
secondPuzzle = sum . map countSummaryWithSmudges . readPatterns <$> input
