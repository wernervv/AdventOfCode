{-# LANGUAGE TupleSections #-}

import Data.List.Split(splitOn)

type Field = [[(Int,Bool)]]
type Coord = (Int,Int)

input :: IO [[Int]]
input = map (map ((read :: String -> Int) . (: []))) . lines <$> readFile "day_11_input.txt"

initialize :: [[Int]] -> Field
initialize = map (map (,False))

initializedInput :: IO Field
initializedInput = initialize <$> input

boundaries :: Field -> Coord
boundaries f = (length (head f) - 1, length f - 1)

isWithinBoundaries :: Coord -> Field -> Bool
isWithinBoundaries (x,y) f = x >= 0 && x <= bx && y >= 0 && y <= by
  where
    (bx, by) = boundaries f

neighbors :: Coord -> Field -> [Coord]
neighbors (x,y) f = filter (`isWithinBoundaries` f) candidates
  where
    candidates = [(newX,newY) | newX <- [(x-1)..(x+1)], newY <- [(y-1)..(y+1)], not (newX == x && newY == y)]

modifyElement :: Coord -> Field -> ((Int,Bool) -> (Int,Bool)) -> Field
modifyElement (0,0) fi fu = let row = head fi in (fu (head row) : tail row): tail fi
modifyElement (x,0) fi fu = let row = head fi in modifyWithinRow x row fu : tail fi
modifyElement (x,y) fi fu = head fi : modifyElement (x,y-1) (tail fi) fu

modifyWithinRow :: Int -> [(Int,Bool)] -> ((Int,Bool) -> (Int,Bool)) -> [(Int,Bool)]
modifyWithinRow 0 row f = f (head row) : tail row
modifyWithinRow n row f = head row : modifyWithinRow (n-1) (tail row) f

-- increaseEnergy :: Coord -> Field -> Field
-- increaseEnergy (0,0) f = let row = head f in (increaseElement (head row) : tail row): tail f
-- increaseEnergy (x,0) f = let row = head f in increaseWithinRow x row : tail f
-- increaseEnergy (x,y) f = head f : increaseEnergy (x,y-1) (tail f)

-- increaseWithinRow :: Int -> [(Int,Bool)] -> [(Int,Bool)]
-- increaseWithinRow 0 row = increaseElement (head row) : tail row
-- increaseWithinRow n row = head row : increaseWithinRow (n-1) (tail row)

increaseElement :: (Int,Bool) -> (Int,Bool)
increaseElement p = (fst p + 1, snd p)

increaseEnergy :: Coord -> Field -> Field
increaseEnergy c f = modifyElement c f increaseElement

getFlashed :: Coord -> Field -> Field
getFlashed c f = modifyElement c f (setFlashStatus True)

increaseAll :: Field -> Field
increaseAll = map $ map increaseElement

getElement :: Coord -> Field -> (Int,Bool)
getElement (x,y) f = f !! y !! x

allAboutToFlash :: Field -> [Coord]
allAboutToFlash f = filter (\ c -> (\ p -> fst p > 9 && not (snd p)) $ getElement c f) allCoords
  where
    allCoords = [(x,y) | x <- [0..bx], y <- [0..by]]
    (bx,by) = boundaries f

setFlashStatus :: Bool -> (Int,Bool) -> (Int,Bool)
setFlashStatus b (x,_) = (x,b)

flashOne :: Coord -> Field -> Field
flashOne c f = getFlashed c $ foldl (flip increaseEnergy) f (neighbors c f)

flash :: Field -> Field
flash f = let intermediateField = foldl (flip flashOne) f (allAboutToFlash f)
              shouldGoOn = not . null $ allAboutToFlash intermediateField
          in if shouldGoOn then flash intermediateField else intermediateField

resetEnergy :: (Int,Bool) -> (Int,Bool)
resetEnergy = const (0, False)

resetFlashed :: Field -> Field
resetFlashed = map (map (\ p -> if snd p then resetEnergy p else p))

-- step :: Field -> Field
-- step = resetFlashed . flash . increaseAll

countFlashesForGivenSteps :: Int -> Field -> Int
countFlashesForGivenSteps = helper 0
  where
    helper acc 0 f = acc
    helper acc n f = let intermediateField = flash . increaseAll $ f
                         flashCount = length . filter snd $ concat intermediateField
                     in helper (acc+flashCount) (n-1) (resetFlashed intermediateField)

firstPart :: IO Int
firstPart = countFlashesForGivenSteps 100 <$> initializedInput

firstSimultaneousFlash :: Field -> Int
firstSimultaneousFlash = helper 1
  where
    helper ctr f = let intermediateField = flash . increaseAll $ f
                       flashCount = length . filter snd $ concat intermediateField
                       (bx,by) = boundaries f
                       totalSize = (bx+1) * (by+1)
                    in if flashCount == totalSize then ctr else helper (ctr+1) (resetFlashed intermediateField)

secondPart :: IO Int
secondPart = firstSimultaneousFlash <$> initializedInput

testInput :: Field
testInput = initialize . map (map ((read :: String -> Int) . (: []))) . splitOn " " $ "5483143223 2745854711 5264556173 6141336146 6357385478 4167524645 2176841721 6882881134 4846848554 5283751526"
