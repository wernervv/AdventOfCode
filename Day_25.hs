import Data.Bifunctor(bimap)

data GridElement = East | South | Free
type Grid = [[GridElement]]
type Coord = (Int,Int)

instance Show GridElement where
  show East = ">"
  show South = "v"
  show Free = "."

printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . concatMap show)

input :: IO Grid
input =  map (map readGridElement) . lines <$> readFile "day_25_input.txt"

readGridElement :: Char -> GridElement
readGridElement c =
  case c of
    '>' -> East
    'v' -> South
    _   -> Free

giveSize :: Grid -> Coord
giveSize g = (x,y)
  where
    x = length . head $ g
    y = length g

minusOne :: Num a => a -> a
minusOne n = n -1

maxCoord :: Grid -> Coord
maxCoord = bimap minusOne minusOne . giveSize

pickElement :: Grid -> Coord -> GridElement
pickElement g (x,y) = g !! y !! x

isFree :: GridElement -> Bool
isFree Free = True
isFree _ = False

nextEast :: Grid -> Coord -> GridElement
nextEast g (x,y) = pickElement g (newX,y)
  where
    maxX = fst $ maxCoord g
    newX = if x == maxX then 0 else x+1

nextSouth :: Grid -> Coord -> GridElement
nextSouth g (x,y) = pickElement g (x,newY)
  where
    maxY = snd $ maxCoord g
    newY = if y == maxY then 0 else y+1

replaceWith :: GridElement -> Coord -> Grid -> Grid
replaceWith el (x,0) g = helper el x (head g) : tail g
  where
    helper el 0 ls = el : tail ls
    helper el x ls = head ls : helper el (x-1) (tail ls)
replaceWith el (x,y) g = head g : replaceWith el (x,y-1) (tail g)

moveEastOne :: Grid -> Coord -> Grid -- Only used when the space is free
moveEastOne g c@(x,y) =
  let maxX = fst $ maxCoord g
      newX = if x == maxX then 0 else x+1
  in replaceWith Free c (replaceWith East (newX,y) g)

moveSouthOne :: Grid -> Coord -> Grid -- Only used when the space is free
moveSouthOne g c@(x,y) =
  let maxY = snd $ maxCoord g
      newY = if y == maxY then 0 else y+1
  in replaceWith Free c (replaceWith South (x,newY) g)

nextColumnwiseIndex :: Coord -> Grid -> Coord
nextColumnwiseIndex c@(x,y) g = if c == maxC then (-1,-1) else (newX,newY)
  where
    maxC@(maxX,maxY) = maxCoord g
    newX = if y == maxY then x+1 else x
    newY = if y == maxY then 0 else y+1

nextRowwiseIndex :: Coord -> Grid -> Coord
nextRowwiseIndex c@(x,y) g = if c == maxC then (-1,-1) else (newX,newY)
  where
    maxC@(maxX,maxY) = maxCoord g
    newX = if x == maxX then 0 else x+1
    newY = if x == maxX then y+1 else y

moveEast :: Grid -> (Grid, Bool)
moveEast = helper (0,0) False
  where
    helper c b g =
      if c == (-1,-1) then (g,b) else
      let (next, newB) = helper (nextColumnwiseIndex c g) b g
      in case pickElement g c of
           East -> if isFree $ nextEast g c then (moveEastOne next c, True) else (next, newB)
           _ -> (next, newB)

moveSouth :: Grid -> (Grid, Bool)
moveSouth = helper (0,0) False
  where
    helper c b g =
      if c == (-1,-1) then (g,b) else
      let (next, newB) = helper (nextRowwiseIndex c g) b g
      in case pickElement g c of
           South -> if isFree $ nextSouth g c then (moveSouthOne next c, True) else (next, newB)
           _ -> (next, newB)

step :: Grid -> (Grid, Bool) -- boolean tells if the grid changes
step g = (sGrid, eBool || sBool)
  where (eGrid, eBool) = moveEast g
        (sGrid, sBool) = moveSouth eGrid

movesUntilHalt :: Grid -> Int
movesUntilHalt = helper 0 . (\ g -> (g,True))
  where
    helper count (g, someOneMoved) =
      if not someOneMoved
        then count
        else helper (count+1) (step g)

firstPart :: IO Int
firstPart = input >>= return . movesUntilHalt

testGrid :: Grid
testGrid = map (map readGridElement) ["v...>>.vv>"
                                     ,".vv>>.vv.."
                                     ,">>.>v>...v"
                                     ,">>v>>.>.v."
                                     ,"v>v.vv.v.."
                                     ,">.>>..v..."
                                     ,".vv..>.>v."
                                     ,"v.v..>>v.v"
                                     ,"....v..v.>"]
