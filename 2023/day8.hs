input :: IO [String]
input = lines <$> readFile "day8_input.txt"

readInstructions :: [String] -> String
readInstructions = (!! 0) 

type Node = (String,(String,String))

readSingleNode :: String -> Node
readSingleNode s =
  let nodeName = takeWhile (/= ' ') s
      leftChild = takeWhile (/= ',') . drop 1 . dropWhile (/= '(') $ s
      rightChild = takeWhile (/= ')') . drop 2 . dropWhile (/= ',') $ s
  in (nodeName, (leftChild, rightChild))

readNodes :: [String] -> [Node]
readNodes = map readSingleNode . drop 1 . dropWhile (/= "")

followOneInstruction :: [Node] -> Node -> Char -> Node
followOneInstruction allNodes (_,(left,right)) instruction =
  let target = if (instruction == 'L') then left else right
  in head . filter (\ (nodeName, _) -> nodeName == target) $ allNodes

countStepsBetween :: String -> [Node] -> String -> String -> Int
countStepsBetween instructions allNodes start finish =
  let startNode = head . filter (\ (nodeName,_) -> nodeName == start) $ allNodes
  in go 0 instructions allNodes startNode finish
    where
      go stepsTaken instructions allNodes current@(name,_) finish =
        if (name == finish)
          then stepsTaken
          else go (stepsTaken+1) instructions allNodes (followOneInstruction allNodes current rightInstruction) finish
            where
              rightInstruction = instructions !! rightIndex
              rightIndex = stepsTaken `mod` length instructions

firstPuzzle :: IO Int
firstPuzzle = do
  inp <- input
  let instructions = readInstructions inp
      allNodes = readNodes inp
  return $ countStepsBetween instructions allNodes "AAA" "ZZZ"

allEndingWithA :: [Node] -> [Node]
allEndingWithA = filter ((== 'A') . last . fst)

endsWithZ :: Node -> Bool
endsWithZ = (== 'Z') . last . fst

type NodeOffset = (Node, Int)

takeOneStep :: String -> [Node] -> NodeOffset -> NodeOffset
takeOneStep instructions allNodes (node, stepsTaken) =
  let indexOfNext = stepsTaken `mod` (length instructions)
      nextInstruction = instructions !! indexOfNext
  in (followOneInstruction allNodes node nextInstruction, stepsTaken + 1)

identifyLoop :: String -> [Node] -> NodeOffset -> (Int, Int)
identifyLoop = go []
  where
    go seen instr allNodes no =
      let nodeName = fst . fst $ no
          steps = snd no
          stepOffset = steps `mod` (length instr)
          currentEntry = (nodeName, stepOffset, steps)
          earlierSame = filter (\ (seenName, seenOffset, _) -> nodeName == seenName && stepOffset == seenOffset) seen
          sameStateSeen = length earlierSame > 0
      in if (sameStateSeen)
           then let (_,_,firstSteps) = head earlierSame
                in (firstSteps, steps - firstSteps)
           else go (currentEntry : seen) instr allNodes (takeOneStep instr allNodes no)

relevantSteps :: String -> [Node] -> (Node -> Bool) -> NodeOffset -> [Int]
relevantSteps instr allNodes p no =
  let loopNums = identifyLoop instr allNodes no
  in reverse $ go instr allNodes p no loopNums 0 []
    where
      go instr allNodes p no ln@(begin,end) n accum =
        if (n > end)
          then accum
          else let newNO = takeOneStep instr allNodes no
               in if (n > begin && p (fst no))
                    then go instr allNodes p newNO ln (n+1) (n:accum)
                    else go instr allNodes p newNO ln (n+1) accum

transitionsToNext :: Int -> [Int] -> [Int]
transitionsToNext loopSize relevant = reverse $ go [] loopSize relevant
  where
    go accum _ [] = accum
    go accum loopSize [x] = loopSize : accum
    go accum loopSize (x:y:xs) =
      let transition = y - x
          reducedLoopSize = loopSize - transition
      in go (transition : accum) reducedLoopSize (y:xs)

transFormNodeOffset :: String -> [Node] -> NodeOffset -> ([Int],(Int,Int))
transFormNodeOffset instr allNodes no =
  let (firstLooping, loopSize) = identifyLoop instr allNodes no
      relevant = relevantSteps instr allNodes endsWithZ no
      transitions = transitionsToNext loopSize relevant
  in (transitions, (head relevant, 0))

giveMorePrimes :: [Int] -> [Int]
giveMorePrimes [] = [2]
giveMorePrimes ps = go ps [(last ps + 1) ..]
  where
    go ps (n:ns) = if (all (\ p -> (n `mod` p) /= 0) ps) then ps ++ [n] else go ps ns

primesUntilOneBigger :: [Int] -> Int -> [Int]
primesUntilOneBigger [] limit = primesUntilOneBigger (giveMorePrimes []) limit
primesUntilOneBigger primes limit =
  if (last primes > limit)
    then primes
    else primesUntilOneBigger (giveMorePrimes primes) limit

givePrimeFactors :: Int -> [Int]
givePrimeFactors n =
  let checkLimit = floor . sqrt . fromIntegral $ n
      primesUntilLimit = init $ primesUntilOneBigger [] checkLimit
  in go primesUntilLimit n
    where
      go [] n = [n]
      go (p:ps) n =
        if (n `mod` p == 0)
          then p : givePrimeFactors (n `div` p)
          else go ps n

factorsForLCM :: [Int] -> [Int] -> [Int]
factorsForLCM [] f2 = f2
factorsForLCM f1 [] = f1
factorsForLCM (p1:ps1) (p2:ps2)
  | p1 == p2  = p1 : factorsForLCM ps1 ps2
  | p1 < p2   = p1 : factorsForLCM ps1 (p2:ps2)
  | otherwise = p2 : factorsForLCM (p1:ps1) ps2

lcmForMultiple :: [Int] -> Int
lcmForMultiple = product . foldl1 factorsForLCM . map givePrimeFactors

secondPuzzle :: IO Int
secondPuzzle = do
  inp <- input
  let instructions = readInstructions inp
      allNodes = readNodes inp
      startState = map (\ node -> head . fst $ transFormNodeOffset instructions allNodes (node, 0)) $ allEndingWithA allNodes
      -- eyeballing reveals that the first value is equal to the loop size for all the values
      -- thus, the problem is solved by finding the lcm of these values
  return $ lcmForMultiple startState

