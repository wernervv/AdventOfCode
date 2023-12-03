import Data.List.Split

input :: IO [String]
input = lines <$> readFile "day2_input.txt"

giveGameID :: String -> Int
giveGameID = read . (!! 1) . words . takeWhile (/= ':')

giveSubsets :: String -> [String]
giveSubsets = split (dropDelims $ oneOf ";") . drop 1 . dropWhile (/= ':')

data Cube = Red Int | Green Int | Blue Int
  deriving (Show)

parseCube :: String -> Cube
parseCube s = let cubeType = amountType !! 1
                  amount = read . head $ amountType
                  amountType = words s
  in case cubeType of
       "red" -> Red amount
       "green" -> Green amount
       "blue" -> Blue amount
       _ -> Red 0

subsetToCubes :: String -> [Cube]
subsetToCubes = map parseCube . split (dropDelims $ oneOf ",")

subsetIsPossible :: [Cube] -> (Int,Int,Int) -> Bool
subsetIsPossible [] _ = True
subsetIsPossible (c:cs) limits@(r,g,b) = case c of
  Red n -> n <= r && subsetIsPossible cs limits
  Green n -> n <= g && subsetIsPossible cs limits
  Blue n -> n <= b && subsetIsPossible cs limits

gameIsPossible :: String -> (Int,Int,Int) -> Bool
gameIsPossible s limits = and . map ((`subsetIsPossible` limits) . subsetToCubes) . giveSubsets $ s

firstPuzzle :: IO Int
firstPuzzle =
  let limits = (12, 13, 14) in
    sum . map giveGameID . filter (`gameIsPossible` limits) <$> input

testInput = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]

minimumsForSubset :: [Cube] -> (Int,Int,Int)
minimumsForSubset = go (0,0,0)
  where
    go initial [] = initial
    go initial (c:cs) = go (updateOneLimit initial c) cs
    updateOneLimit (r,g,b) c = case c of
      Red n -> (max r n, g, b)
      Green n -> (r, max g n, b)
      Blue n -> (r, g, max b n)

updateLimits :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
updateLimits (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

minimumsForGame :: String -> (Int,Int,Int)
minimumsForGame = foldl1 updateLimits . map (minimumsForSubset . subsetToCubes) . giveSubsets 

calculatePower :: (Int,Int,Int) -> Int
calculatePower (r,g,b) = r * g * b

secondPuzzle :: IO Int
secondPuzzle = sum . map (calculatePower . minimumsForGame) <$> input

