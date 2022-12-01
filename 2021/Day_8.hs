import Data.List.Split
import Data.List((\\))

type Notes = [([String], [String])]

input :: IO Notes
input = map ((\ [x,y] -> (splitOn " " x, splitOn " " y)) . splitOn " | ") . lines <$> readFile "day_8_input.txt"

allOutput :: Notes -> [[String]]
allOutput = map snd

isOne :: String -> Bool
isOne = (== 2) . length

isFour :: String -> Bool
isFour = (== 4) . length

isSeven :: String -> Bool
isSeven = (== 3) . length

isEight :: String -> Bool
isEight = (== 7) . length

isEasyDigit :: String -> Bool
isEasyDigit str = isOne str || isFour str || isSeven str || isEight str

firstPart :: IO Int
firstPart = length . filter isEasyDigit . concat . allOutput <$> input

containsAll :: String -> String -> Bool
containsAll digits str = all (`elem` str) digits

leftArc :: [String] -> String
leftArc observations = filter (not . (`elem` one observations)) (four observations)

zero :: [String] -> String
zero observations = head $ filter (\ el -> length el == 6 && not (containsAll (leftArc observations) el)) observations

one :: [String] -> String
one = head . filter isOne

two :: [String] -> String
two observations = head $ filter (\ el -> length el == 5 && not (null (el \\ five observations)) && not (null (el \\ three observations))) observations

three :: [String] -> String
three observations = head $ filter (\ el -> length el == 5 && length (el \\ five observations) == 1) observations

four :: [String] -> String
four = head . filter isFour

five :: [String] -> String
five observations = head $ filter (\ el -> containsAll (leftArc observations) el && length el == 5) observations

six :: [String] -> String
six observations = head $ filter (\ el -> length el == 6 && not (null (one observations \\ el))) observations

seven :: [String] -> String
seven = head . filter isSeven

eight :: [String] -> String
eight = head . filter isEight

nine :: [String] -> String
nine observations = head $ filter (\ el -> length el == 6 && not (null (el \\ six observations)) && not (null (el \\ zero observations))) observations

signalPatterns :: ([String], [String]) -> [String]
signalPatterns = fst

output :: ([String], [String]) -> [String]
output = snd

areSame :: String -> String -> Bool
areSame a b = null (a \\ b) && null (b \\ a)

solveOne :: ([String], [String]) -> Int
solveOne input = (read :: String -> Int) $ map (\ x -> snd $ head $ filter (areSame x . fst) orderedObservations) (output input)
  where
    orderedObservations = zip (map ($ signalPatterns input) [zero, one, two, three, four, five, six, seven, eight, nine]) ['0'..'9']

secondPart :: IO Int
secondPart = sum . map solveOne <$> input

testInput :: ([String], [String])
testInput = (["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"], ["cdfeb", "fcadb", "cdfeb", "cdbaf"])
