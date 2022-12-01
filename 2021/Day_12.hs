{-# LANGUAGE TupleSections #-}

import Data.Char(isAsciiLower,isAsciiUpper)
import Data.List(group,sort)
import Data.List.Split(splitOn)

type Connection = (String,String)
type Cave = String
type CaveState = [(Cave,Bool)]
type Path = [Cave]

input :: IO [Connection]
input = map readConnection . lines <$> readFile "day_12_input.txt"

readConnection :: String -> Connection
readConnection = (\ [x,y] -> (x,y)) . splitOn "-"

conditionalAdd :: Eq a => a -> [a] -> [a]
conditionalAdd el ls =
  if el `elem` ls
    then ls
    else el : ls

allCaves :: [Connection] -> [Cave]
allCaves = helper []
  where
    helper caves [] = caves
    helper caves ((fromCave,toCave):rest) = helper (conditionalAdd toCave (conditionalAdd fromCave caves)) rest

initCaveState :: [Cave] -> CaveState
initCaveState = arrive "start" . map (,False)

-- isBigCave :: Cave -> Bool
-- isBigCave cave = let c = head cave
--                  in isAsciiUpper c

isSmallCave :: Cave -> Bool
isSmallCave cave = let c = head cave
                   in isAsciiLower c

isWorthExploring :: (Cave,Bool) -> Bool
isWorthExploring = not . snd

startCave :: CaveState -> (Cave,Bool)
startCave = head . filter ((== "start") . fst)

areConnected :: Cave -> Cave -> [Connection] -> Bool
areConnected c1 c2 = any (\p -> (fst p == c1 && snd p == c2) || (fst p == c2 && snd p == c1))

arrive :: Cave -> CaveState -> CaveState
arrive c = map (\ orig@(cave,_) -> if cave == c then (cave,isSmallCave cave) else orig)

possibleTransitions :: Cave -> [Connection] -> CaveState -> [Cave]
possibleTransitions c lc =
  if c == "end" then const []
  else map fst . filter (\ oneCave -> areConnected c (fst oneCave) lc && isWorthExploring oneCave)

allPathsDirty :: [Connection] -> [Path]
allPathsDirty lc =
  let cs = initCaveState . allCaves $ lc
  in helper ["start"] "start" lc cs
    where
      helper path currentCave lc cs =
        let nextCaves = possibleTransitions currentCave lc cs
        in if null nextCaves
             then [path]
             else concatMap moveToNextCave nextCaves
               where
                 moveToNextCave c = helper (c : path) c lc (arrive c cs)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

prunePaths :: [Path] -> [Path]
prunePaths = map reverse . removeDuplicates . filter (\ path -> head path == "end")

firstPart :: IO Int
firstPart = length . prunePaths . allPathsDirty <$> input

updateVisitPossibility :: Bool -> CaveState -> CaveState
updateVisitPossibility b = map (\ orig@(cave,_) -> if cave == "secondVisitUsed" && b then (cave, True) else orig)

arrive2 :: Bool -> Cave -> CaveState -> CaveState
arrive2 visitTwice c =
  let getsVisited = not visitTwice
  in updateVisitPossibility visitTwice . map (\ orig@(cave,_) -> if cave == c then (cave,getsVisited && isSmallCave cave) else orig)

secondVisitIsUsable :: CaveState -> Bool
secondVisitIsUsable = not . snd . head . filter ((== "secondVisitUsed" ) . fst)

allPathsVisitPossibility :: [Connection] -> [Path]
allPathsVisitPossibility lc =
  let cs = (("secondVisitUsed",False):) . initCaveState . allCaves $ lc
  in helper ["start"] "start" lc cs
    where
      helper path currentCave lc cs =
        let nextCaves = possibleTransitions currentCave lc cs
        in if null nextCaves
             then [path]
             else concatMap conditionalMove nextCaves
                    where
                      conditionalMove c =
                        let canMakeSecondVisit = secondVisitIsUsable cs
                            conditionalPaths = if canMakeSecondVisit then helper (c : path) c lc (arrive2 True c cs) else []
                        in helper (c : path) c lc (arrive2 False c cs) ++ conditionalPaths

secondPart :: IO Int
secondPart = length . prunePaths . allPathsVisitPossibility <$> input

testInput :: [Connection]
testInput = map readConnection $ splitOn " " "start-A start-b A-c A-b b-d A-end b-end"

slightlyLarger :: [Connection]
slightlyLarger = map readConnection $ splitOn " " "dc-end HN-start start-kj dc-start dc-HN LN-dc HN-end kj-sa kj-HN kj-dc"

evenLarger :: [Connection]
evenLarger = map readConnection $ splitOn " " "fs-end he-DX fs-he start-DX pj-DX end-zg zg-sl zg-pj pj-he RW-he fs-DX pj-RW zg-RW start-pj he-WI zg-he pj-fs start-RW"
