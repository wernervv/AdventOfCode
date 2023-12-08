input :: IO [String]
input = lines <$> readFile "day8_input.txt"

readInstructions :: [String] -> String
readInstructions = concat . repeat . (!! 0) 

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
      go stepsTaken (i:is) allNodes current@(name,_) finish =
        if (name == finish)
          then stepsTaken
          else go (stepsTaken+1) is allNodes (followOneInstruction allNodes current i) finish

firstPuzzle :: IO Int
firstPuzzle = do
  inp <- input
  let instructions = readInstructions inp
      allNodes = readNodes inp
  return $ countStepsBetween instructions allNodes "AAA" "ZZZ"

