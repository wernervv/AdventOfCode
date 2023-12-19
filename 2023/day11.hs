input :: IO [String]
input = lines <$> readFile "day11_input.txt"

type Image = [[Char]]

type Galaxy = (Int,Int)

type EmptySpaces = ([Int], [Int])

readGalaxiesInRow :: [Char] -> Int -> [Galaxy]
readGalaxiesInRow = go [] 0
  where
    go accum _ [] _ = accum
    go accum ind (c:cs) row =
      let newAccum = if (c == '#') then (ind,row) : accum else accum
      in go newAccum (ind+1) cs row

readGalaxies :: Image -> [Galaxy]
readGalaxies i = concat $ zipWith readGalaxiesInRow i [0..]

isEmpty :: [Char] -> Bool
isEmpty = not . (elem '#')

readEmptyColumns :: Image -> [Int]
readEmptyColumns i =
  let maxX = length (i !! 0) - 1
      maxY = length i - 1
      giveColumn i x = map (\ y -> i !! y !! x) [0..maxY]
  in filter (\ index -> isEmpty (giveColumn i index)) [0..maxX]

readEmptyRows :: Image -> [Int]
readEmptyRows i =
  let maxY = length i - 1
  in filter (\ y -> isEmpty (i !! y)) [0..maxY]

readEmptySpaces :: Image -> EmptySpaces
readEmptySpaces i = (readEmptyColumns i, readEmptyRows i)

giveAllBetween :: Ord a => [a] -> a -> a -> [a]
giveAllBetween xs start end = filter (\ x -> x > start && x < end) xs

additionFromExpanding :: EmptySpaces -> Galaxy -> Galaxy -> Int
additionFromExpanding (colIndices,rowIndices) (x1,y1) (x2,y2) =
  let columnsBetween = if (x2 > x1) then giveAllBetween colIndices x1 x2 else giveAllBetween colIndices x2 x1
      rowsBetween = if (y2 > y1) then giveAllBetween rowIndices y1 y2 else giveAllBetween rowIndices y2 y1
  in length columnsBetween + length rowsBetween

distanceBetweenGalaxies :: EmptySpaces -> Galaxy -> Galaxy -> Int
distanceBetweenGalaxies es g1@(x1,y1) g2@(x2,y2) =
  let xDiff = abs (x2 - x1)
      yDiff = abs (y2 - y1)
      effectOfExpansion = additionFromExpanding es g1 g2
  in xDiff + yDiff + effectOfExpansion

giveAllPairs :: [a] -> [(a,a)]
giveAllPairs [] = []
giveAllPairs (x:xs) = map (\ y -> (x,y)) xs ++ giveAllPairs xs

firstPuzzle :: IO Int
firstPuzzle =
  do image <- input
     let galaxies = readGalaxies image
         allGalaxyPairs = giveAllPairs galaxies
         emptySpaces = readEmptySpaces image
     return $ sum . (map (uncurry (distanceBetweenGalaxies emptySpaces))) $ allGalaxyPairs
