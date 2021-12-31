import Data.List.Split(splitOn)

type Instruction = (Bool,(Int,Int),(Int,Int),(Int,Int)) -- turn on, x, y, z

input :: IO [String]
input = lines <$> readFile "day_22_input.txt"

parseInstruction :: String -> Instruction
parseInstruction s =
  let [onOrOff,rest] = splitOn " " s
      [x,y,z] = splitOn "," rest
      [rangeX,rangeY,rangeZ] = map ((\ [x,y] -> (x,y)) . map (read :: String -> Int) . splitOn ".." . drop 2) [x,y,z]
  in (onOrOff == "on",rangeX,rangeY,rangeZ)

rangeIsPartOfInitialization :: (Int,Int) -> Bool
rangeIsPartOfInitialization (start,end) = start >= -50 && end <= 50

isPartOfInitialization :: Instruction -> Bool
isPartOfInitialization (_,rx,ry,rz) = rangeIsPartOfInitialization rx && rangeIsPartOfInitialization ry && rangeIsPartOfInitialization rz

instructionsFirstPart :: IO [Instruction]
instructionsFirstPart = filter isPartOfInitialization . map parseInstruction <$> input
