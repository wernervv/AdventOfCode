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
