import Data.List.Split (splitOn)

input :: IO [String]
input = lines <$> readFile "day12_input.txt"

type Record = (String,[Int])

readRecord :: String -> Record
readRecord s = (stringPart, numPart)
  where
    splitString = splitOn " " s
    stringPart = splitString !! 0
    numPart = map read . splitOn "," $ splitString !! 1


validSoFar :: Record -> String -> Bool
validSoFar (str, nums) s = groupsAreValid strippedCombined nums
  where
    groupsAreValid [] [] = True
    groupsAreValid ('#':_) [] = False
    groupsAreValid (c:cs) [] = groupsAreValid cs []
    groupsAreValid [] [0] = True
    groupsAreValid [] ns = False
    groupsAreValid ('#':cs) (0:ns) = False
    groupsAreValid ('#':cs) (n:ns) = groupsAreValid cs (n-1:ns)
    groupsAreValid ('.':cs) (0:ns) = groupsAreValid (dropWhile (== '.') cs) ns
    groupsAreValid ('.':_) ns = False
    groupsAreValid _ _ = True
    combineToFull [] [] = []
    combineToFull str [] = str
    combineToFull [] s = s
    combineToFull ('?':str) (c:s) = c : combineToFull str s
    combineToFull (c:str) s = c : combineToFull str s
    combined = combineToFull str s
    strippedCombined = dropWhile (== '.') combined

allFilled :: String -> Bool
allFilled = all (/= '?')

addSpring :: String -> String
addSpring [] = []
addSpring ('?':cs) = '#' : cs
addSpring (c:cs) = c : addSpring cs

backtrack :: String -> Maybe String
backtrack s = if (changeWasMade) then Just backtracked else Nothing
  where
    undoLatest '.' (cs, False) = ('?':cs, False)
    undoLatest '#' (cs, False) = ('.':cs, True)
    undoLatest c (cs, latestUndone) = (c:cs, latestUndone)
    (backtracked, changeWasMade) = foldr undoLatest ([],False) s

iterateUntilValid :: Record -> String -> Maybe String
iterateUntilValid rec s =
  if (validSoFar rec s)
    then if (allFilled s)
           then Just s
           else iterateUntilValid rec (addSpring s)
    else case backtrack s of
           (Just backtracked) -> iterateUntilValid rec backtracked
           Nothing -> Nothing

giveFirstValid :: Record -> Maybe String
giveFirstValid rec = iterateUntilValid rec initial
  where initial = filter (== '?') . fst $ rec

giveNextValid :: Record -> String -> Maybe String
giveNextValid rec s = backtrack s >>= iterateUntilValid rec

countAllValid :: Record -> Int
countAllValid rec =
  let first = giveFirstValid rec
  in case first of
    Nothing -> 0
    s -> count 1 rec s
      where
        count n rec s = case s >>= giveNextValid rec of
          Nothing -> n
          ms -> count (n+1) rec ms

firstPuzzle :: IO Int
firstPuzzle = sum . map (countAllValid . readRecord) <$> input

testInput :: [String]
testInput = ["???.### 1,1,3",
  ".??..??...?##. 1,1,3",
  "?#?#?#?#?#?#?#? 1,3,1,6",
  "????.#...#... 4,1,1",
  "????.######..#####. 1,6,5",
  "?###???????? 3,2,1"]

