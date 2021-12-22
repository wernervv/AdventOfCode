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

isInArea :: Coord -> Area -> Bool
isInArea (x,y) (x1,x2,y1,y2) = x >= x1 && x <= x2 && y >= y1 && y <= y2

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

maxYVelocity :: Area -> Int
maxYVelocity (_,_,y,_) =
  let startY = snd start
  in startY - y -1

highestPoint :: Int -> Int
highestPoint yVelocity = snd start + sum [1..yVelocity]

firstPart :: IO Int
firstPart = highestPoint . maxYVelocity <$> input
