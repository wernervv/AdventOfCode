type Coord = (Int, Int)
type Field = [[Int]]

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
