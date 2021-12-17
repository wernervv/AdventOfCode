import Data.Bifunctor(second)
import Data.List.Split(splitOn)

type Coord = (Int,Int)
type Instruction = (Char,Int)

data PaperUnit = Dot | Blank
type Paper = [[PaperUnit]]

input :: IO [String]
input = lines <$> readFile "day_13_input.txt"

refinedInput :: IO ([Coord],[Instruction])
refinedInput = do ls <- input
                  let (coordStr,instrStr) = splitInput ls
                  return (locations coordStr, foldInstructions instrStr)

locations :: [String] -> [Coord]
locations = map ((\ [x,y] -> (x,y)) . map (read :: String -> Int) . splitOn ",")

oneInstruction :: String -> Instruction
oneInstruction s =
  case drop 11 s of
    ('x':_:ln) -> ('x', (read :: String -> Int) ln)
    ('y':_:ln) -> ('y', (read :: String -> Int) ln)
    _         -> ('1',1)

foldInstructions :: [String] -> [Instruction]
foldInstructions = map oneInstruction

splitInput :: [String] -> ([String],[String])
splitInput = second tail . break null

paperSize :: [Coord] -> Coord
paperSize lc = (maxX,maxY)
  where
    maxX = maximum . map fst $ lc
    maxY = maximum . map snd $ lc

createPaper :: [Coord] -> Paper
createPaper lc = let (x,y) = paperSize lc
                 in replicate y $ replicate x Blank

drawOneDot :: Coord -> Paper -> Paper
drawOneDot (0,0) p = let row = head p in (Dot : tail row): tail p
drawOneDot (x,0) p = let row = head p in drawWithinRow x row : tail p
drawOneDot (x,y) p = head p : drawOneDot (x,y-1) (tail p)

drawWithinRow :: Int -> [PaperUnit] -> [PaperUnit]
drawWithinRow 0 row = Dot : tail row
drawWithinRow n row = head row : drawWithinRow (n-1) (tail row)

drawDots :: [Coord] -> Paper
drawDots lc = foldl (flip ($)) (createPaper lc) (map drawOneDot lc)

splitOnFold :: Int -> [Coord] -> ([Coord],[Coord])
splitOnFold = helper []
  where
    helper acc 0 coords = (acc, coords)
    helper acc n (c:cs) = helper (c:acc) (n-1) cs
    helper acc n [] = ([],[])

foldHorizontal :: [Coord] -> [Coord]
foldHorizontal = undefined
