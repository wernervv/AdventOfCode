import Data.List.Split(splitOn)

type Instruction = (Bool,(Int,Int),(Int,Int),(Int,Int)) -- turn on, x, y, z
type Coord = (Int,Int,Int)

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

isOn :: Instruction -> Bool
isOn (b,_,_,_) = b

isInRange :: (Int,Int) -> Int -> Bool
isInRange (minC,maxC) c = c >= minC && c <= maxC

instructionApplies :: Instruction -> Coord -> Bool
instructionApplies (_,rx,ry,rz) (x,y,z) = isInRange rx x && isInRange ry y && isInRange rz z

lastInstructionConcerning :: [Instruction] -> Coord -> Maybe Instruction
lastInstructionConcerning instr c =
  let allConcerningCoord = filter (`instructionApplies` c) instr
  in if null allConcerningCoord then Nothing else Just . last $ allConcerningCoord

isOnAfterInstructions :: [Instruction] -> Coord -> Bool
isOnAfterInstructions instructions c = maybe False isOn (lastInstructionConcerning instructions c)

firstPart :: IO Int
firstPart =
  do instructions <- instructionsFirstPart
     return . length . filter (isOnAfterInstructions instructions) $ [ (x,y,z) | x <- initializationRange, y <- initializationRange, z <- initializationRange]
       where
         initializationRange = [-50..50]
