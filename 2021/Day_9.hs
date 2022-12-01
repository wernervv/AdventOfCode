import Data.List(group, sort, sortBy, union)

type Field = [[Int]]
type Coord = (Int, Int)

input :: IO Field
input = map (map ((read :: String -> Int) . (: []))) . lines <$> readFile "day_9_input.txt"

boundaries :: Field -> Coord
boundaries f = (length (head f) - 1, length f - 1)

giveHeight :: Coord -> Field -> Int
giveHeight (x,y) f = f !! y !! x

neighbors :: Coord -> Coord -> [Coord]
neighbors (x,y) boundaries = filter (isWithinBoundaries boundaries) [(x-1,y), (x,y-1), (x,y+1), (x+1,y)]

isWithinBoundaries :: Coord -> Coord -> Bool
isWithinBoundaries (bx,by) (x,y) = x >= 0 && x <= bx && y >= 0 && y <= by

isLowPoint :: Coord -> Field -> Bool
isLowPoint c f = all ((giveHeight c f <) . flip giveHeight f) (neighbors c (boundaries f))

allLowPoints :: Field -> [Coord]
allLowPoints f = filter (`isLowPoint` f) [(x,y) | x <- [0..bx], y <- [0..by]]
  where
    (bx,by) = boundaries f

firstPart :: IO Int
firstPart =
  do f <- input
     return $ sum $ map ((+1) . (`giveHeight` f)) $ allLowPoints f

increaseBasin :: [Coord] -> Field -> [Coord]
increaseBasin basin f =
  let new = removeDuplicates $ concatMap (filter (\ c -> isPartOfBasin c f && notElem c basin) . (`neighbors` boundaries f)) basin
  in if null new
       then basin
       else increaseBasin (basin ++ new) f

isPartOfBasin :: Coord -> Field -> Bool
isPartOfBasin c f = giveHeight c f /= 9

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

allBasins :: Field -> [[Coord]]
allBasins f = map ((`increaseBasin` f) . (: [])) $ allLowPoints f

basinSize :: [Coord] -> Int
basinSize = length

largestThree :: [Int] -> [Int]
largestThree = take 3 . sortBy (flip compare)

secondPart :: IO Int
secondPart =
  do f <- input
     return $ product . largestThree . map basinSize $ allBasins f

testInput :: Field
testInput = map (map ((read :: String -> Int) . (: []))) ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]
