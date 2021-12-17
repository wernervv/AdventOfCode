import Data.Bifunctor(first,second)
import Data.List.Split(splitOn)
import Data.List(transpose)

type Coord = (Int,Int)
type Instruction = (Char,Int)

data PaperUnit = Dot | Blank
type Paper = [[PaperUnit]]

instance Eq PaperUnit where
  Dot == Dot = True
  Blank == Blank = True
  _ == _ = False

instance Show PaperUnit where
  show Dot = "#"
  show Blank = "."

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
    maxX = (+1) . maximum . map fst $ lc
    maxY = (+1) . maximum . map snd $ lc

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

splitOnFold :: Int -> Paper -> (Paper,Paper)
splitOnFold = helper []
  where
    helper acc 0 paperSplit = (acc, tail paperSplit)
    helper acc n (u:us) = helper (u:acc) (n-1) us
    helper acc n [] = ([],[])

appendWithNElems :: Int -> a -> [a] -> [a]
appendWithNElems n el ls = ls ++ replicate n el

appendToMatchingLengthsWith :: a -> ([a],[a]) -> ([a],[a])
appendToMatchingLengthsWith el (a,b)
  | diff == 0 = (a,b)
  | diff < 0 = (appendWithNElems (-diff) el a, b)
  | diff > 0 = (a, appendWithNElems diff el b)
    where diff = length a - length b

matchDots :: PaperUnit -> PaperUnit -> PaperUnit
matchDots Blank Blank = Blank
matchDots _ _ = Dot

matchPapers :: Paper -> Paper -> Paper
matchPapers [] [] = []
matchPapers (x:xs) (y:ys) = zipWith matchDots x y : matchPapers xs ys

foldHorizontal :: Int -> Paper -> Paper
foldHorizontal line p =
  let halves = splitOnFold line p
      lineLength = length . head $ p
      appendedHalves = appendToMatchingLengthsWith (replicate lineLength Blank) halves
  in reverse $ uncurry matchPapers appendedHalves

foldVertical :: Int -> Paper -> Paper
foldVertical line p = transpose . foldHorizontal line $ transpose p

fold :: Instruction -> Paper -> Paper
fold (dir,lineN) = if dir == 'x' then foldVertical lineN else foldHorizontal lineN

countDots :: Paper -> Int
countDots = sum . map (length . filter (== Dot))

firstPart :: IO Int
firstPart =
  do (coordList, instrList) <- refinedInput
     let paper = drawDots coordList
         instr = head instrList
     return $ countDots $ fold instr paper

secondPart :: IO Paper
secondPart =
  do (coordList, instrList) <- refinedInput
     let paper = drawDots coordList
     return $ foldl (flip fold) paper instrList

-- [[#,.,.,#,.,#,#,#,#,.,#,.,.,#,.,#,.,.,#,.,#,#,#,#,.,#,#,#,#,.,.,.,#,#,.,#,#,#,#,.]
-- ,[#,.,.,#,.,.,.,.,#,.,#,.,#,.,.,#,.,.,#,.,#,.,.,.,.,#,.,.,.,.,.,.,.,#,.,.,.,.,#,.]
-- ,[#,#,#,#,.,.,.,#,.,.,#,#,.,.,.,#,#,#,#,.,#,#,#,.,.,#,#,#,.,.,.,.,.,#,.,.,.,#,.,.]
-- ,[#,.,.,#,.,.,#,.,.,.,#,.,#,.,.,#,.,.,#,.,#,.,.,.,.,#,.,.,.,.,.,.,.,#,.,.,#,.,.,.]
-- ,[#,.,.,#,.,#,.,.,.,.,#,.,#,.,.,#,.,.,#,.,#,.,.,.,.,#,.,.,.,.,#,.,.,#,.,#,.,.,.,.]
-- ,[#,.,.,#,.,#,#,#,#,.,#,.,.,#,.,#,.,.,#,.,#,.,.,.,.,#,#,#,#,.,.,#,#,.,.,#,#,#,#,.]]

testCoords :: [Coord]
testCoords = locations $ splitOn " " "6,10 0,14 9,10 0,3 10,4 4,11 6,0 6,12 4,1 0,13 10,12 3,4 3,0 8,4 1,10 2,14 8,10 9,0"

testInstructions :: [Instruction]
testInstructions = [('y',7), ('x',5)]

testInput :: ([Coord], [Instruction])
testInput = (testCoords, testInstructions)
