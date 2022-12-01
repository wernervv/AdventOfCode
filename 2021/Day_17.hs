import Data.Bifunctor(bimap)
import Data.List.Split(splitOn)

type Area = (Int,Int,Int,Int)
type Coord = (Int,Int)
type Velocity = (Int,Int)

input :: IO Area
input = readRanges . head . lines <$> readFile "day_17_input.txt"

readRanges :: String -> Area
readRanges s =
  let rangesString = splitOn ": " s !! 1
      xyStrings = splitOn ", " rangesString
      [(x1,x2),(y1,y2)] = map extractRange xyStrings
  in (x1,x2,y1,y2)

extractRange :: String -> (Int,Int)
extractRange s =
  let rangeString = drop 2 s
      rangeAsList = map (read :: String -> Int) $ splitOn ".." rangeString
  in (\ [x,y] -> (min x y,max x y)) rangeAsList

start :: Coord
start = (0,0)

maxYVelocity :: Area -> Int
maxYVelocity (_,_,y,_) =
  let startY = snd start
  in startY - y -1

highestPoint :: Int -> Int
highestPoint yVelocity = snd start + sum [1..yVelocity]

firstPart :: IO Int
firstPart = highestPoint . maxYVelocity <$> input

minYVelocity :: Area -> Int
minYVelocity (_,_,y,_) = y - snd start

maxXVelocity :: Area -> Int
maxXVelocity (_,x,_,_) = x - fst start

minXVelocity :: Area -> Int
minXVelocity (x,_,_,_) = helper 1 (x - fst start)
  where
    helper stepSize stillUncovered =
      let newUncovered = stillUncovered - stepSize
          newStepSize = stepSize + 1 -- target area assumed in positive x direction
      in if newUncovered <= 0 then stepSize else helper newStepSize newUncovered

allCandidateVelocities :: Area -> [Velocity]
allCandidateVelocities a = [(vx,vy) | vx <- [(minXVelocity a) .. (maxXVelocity a)], vy <- [(minYVelocity a) .. (maxYVelocity a)]]

minmax :: Char -> Area -> Coord
minmax c (x1,x2,y1,y2) =
  case c of
    'x' -> (x1,x2)
    'y' -> (y1,y2)
    _   -> (0,0)

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f

isUnderTarget :: Coord -> Area -> Bool
isUnderTarget (_,y) (_,_,ay,_) = y < ay

isInArea :: Area -> Coord -> Bool
isInArea (x1,x2,y1,y2) (x,y) = x >= x1 && x <= x2 && y >= y1 && y <= y2

hasHitTarget :: Area -> [(Coord,Velocity)] -> Bool
hasHitTarget area = any (isInArea area . fst)

isValidInitialVelocity :: Velocity -> Area -> Bool
isValidInitialVelocity v a =
  let trajectory = takeWhile (not . (`isUnderTarget` a) . fst) $ iterate stepTrajectory (start,v)
  in hasHitTarget a trajectory

stepTrajectory :: (Coord,Velocity) -> (Coord,Velocity)
stepTrajectory ((x,y),(vx,vy)) =
  let newX = x + vx
      newY = y + vy
      newVx
        | vx > 0 = vx-1
        | vx < 0 = vx+1
        | otherwise = 0
      newVy = vy-1
  in ((newX,newY),(newVx,newVy))

secondPart :: IO Int
secondPart =
  do area <- input
     return $ length . filter (`isValidInitialVelocity` area) $ allCandidateVelocities area