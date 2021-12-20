import Data.Bifunctor(second)
import Data.List(group,sort)
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe)

type Template = String
type Rule = ((Char,Char),Char)
type Counts = [(Char,Int)]

input :: IO [String]
input = lines <$> readFile "day_14_input.txt"

getTemplate :: [String] -> String
getTemplate = head

getRules :: [String] -> [Rule]
getRules = map ((\ [x,y] -> ((\ [a,b] -> (a,b)) x, head y)) . splitOn " -> ") . drop 2

refinedInput :: IO (Template, [Rule])
refinedInput =
  do allLines <- input
     let templ = getTemplate allLines
         rules = getRules allLines
     return (templ, rules)

findMatching :: Char -> Char -> [Rule] -> (Char,Bool)
findMatching start end rules =
  let matchingList = filter ((== (start,end)) . fst) rules
  in if null matchingList then ('a',False) else (snd . head $ matchingList, True)

insertChar :: Char -> Char -> [Rule] -> String
insertChar start end rules =
  let (c,b) = findMatching start end rules
  in if b then [start,c] else [start]

step :: Template -> [Rule] -> Template
step []         _     = []
step [x]        _     = [x]
step (x:y:rest) rules = insertChar x y rules ++ step (y:rest) rules

applyNSteps :: Int -> Template -> [Rule] -> Template
applyNSteps 0 t _ = t
applyNSteps n t rules = applyNSteps (n-1) (step t rules) rules

countValue :: Template -> Int
countValue t = let quantities = map length . group . sort $ t
               in maximum quantities - minimum quantities

firstPart :: IO Int
firstPart =
  do (t,rules) <- refinedInput
     return $ countValue $ applyNSteps 10 t rules

addCharToCounts :: Char -> Counts -> Counts
addCharToCounts = helper []
  where
    helper acc c [] = (c,1) : acc
    helper acc c (p:ps) = if fst p == c then ((c,snd p + 1) : acc) ++ ps else helper (p:acc) c ps

charCountAllButLast :: Counts -> Template -> Counts
charCountAllButLast counts [] = counts
charCountAllButLast counts [x] = counts
charCountAllButLast counts (c:cs) = charCountAllButLast (addCharToCounts c counts) cs

allInterestingPairs :: [Rule] -> [Template]
allInterestingPairs = map ((\ (x,y) -> [x,y]) . fst)

inTenRoundsEntry :: Template -> [Rule] -> (Template,Template)
inTenRoundsEntry t rules = (t,applyNSteps 10 t rules)

inTenRoundsTable :: [Rule] -> [(Template,Template)]
inTenRoundsTable rules = map (`inTenRoundsEntry` rules) (allInterestingPairs rules)

inTenRoundsCountsTable :: [(Template,Template)] -> [(Template,Counts)]
inTenRoundsCountsTable = map (second $ charCountAllButLast [])

templateLookup :: Template -> [(Template,Template)] -> Template
templateLookup pair table = fromMaybe pair (lookup pair table)

countsLookup :: Template -> [(Template,Counts)] -> Counts
countsLookup pair table =
  let firstChar = head pair
      secondChar = pair !! 1
      onlyPairCount = addCharToCounts secondChar (addCharToCounts firstChar [])
  in fromMaybe onlyPairCount (lookup pair table)

allPairs :: Template -> [Template]
allPairs [] = []
allPairs [x] = []
allPairs (x:y:rest) = [x,y] : allPairs (y:rest)

combineOneCount :: Counts -> (Char,Int) -> Counts
combineOneCount [] c = [c]
combineOneCount (p:ps) (c,n) = if fst p == c then (c, snd p + n) : ps else p : combineOneCount ps (c,n)

combineCounts :: Counts -> Counts -> Counts
combineCounts = foldl combineOneCount

addTenRoundsToCounts :: [(Template,Template)] -> [(Template,Counts)] -> [(Template,Counts)]
addTenRoundsToCounts templateTable countsTable = map (second $ foldl1 combineCounts . map (`countsLookup` countsTable) . allPairs) templateTable

inFortyRoundsCountsTable :: [(Template,Template)] -> [(Template,Counts)] -> [(Template,Counts)]
inFortyRoundsCountsTable templateTable countsTable = foldl (flip addTenRoundsToCounts) countsTable (replicate 3 templateTable)

checkSum :: Counts -> Int
checkSum counts =
  let onlyNums = map snd counts
  in maximum onlyNums - minimum onlyNums

secondPart :: IO Int
secondPart =
  do (t,rules) <- refinedInput
     let templateTable = inTenRoundsTable rules
         countsTableTen = inTenRoundsCountsTable templateTable
         countsTableForty = inFortyRoundsCountsTable templateTable countsTableTen
         nearlyCompleteCounts = foldl1 combineCounts $ map (`countsLookup` countsTableForty) (allPairs t)
         completeCounts = addCharToCounts (last t) nearlyCompleteCounts
     return $ checkSum completeCounts

testTemplate :: String
testTemplate = "NNCB"

testRules :: [Rule]
testRules = map ((\ [x,y] -> ((\ [a,b] -> (a,b)) x, head y)) . splitOn " -> ") ["CH -> B" ,"HH -> N" ,"CB -> H" ,"NH -> C" ,"HB -> C" ,"HC -> B" ,"HN -> C" ,"NN -> C" ,"BH -> H" ,"NC -> B" ,"NB -> B" ,"BN -> B" ,"BB -> N" ,"BC -> B" ,"CC -> N" ,"CN -> C"]
