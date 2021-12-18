import Data.List(group,sort)
import Data.List.Split(splitOn)

type Template = String
type Rule = ((Char,Char),Char)

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

testTemplate :: String
testTemplate = "NNCB"

testRules :: [Rule]
testRules = map ((\ [x,y] -> ((\ [a,b] -> (a,b)) x, head y)) . splitOn " -> ") ["CH -> B" ,"HH -> N" ,"CB -> H" ,"NH -> C" ,"HB -> C" ,"HC -> B" ,"HN -> C" ,"NN -> C" ,"BH -> H" ,"NC -> B" ,"NB -> B" ,"BN -> B" ,"BB -> N" ,"BC -> B" ,"CC -> N" ,"CN -> C"]
