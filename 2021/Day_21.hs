{-# LANGUAGE TupleSections #-}

import Data.Bifunctor(bimap,first,second)
import Data.List(group,sort)
import Data.Maybe(fromMaybe)

type PlayersStatus = (Player,Player,Bool) -- player 1, player 2, player 1 is in turn
type Player = (Int,Int) -- position, points

type DeterministicDice = (Int,Int) -- value, times rolled

type Game = (PlayersStatus, DeterministicDice)

type Path = (Player,Int)
type Paths = [Path]

input :: IO [String]
input = lines <$> readFile "day_21_input.txt"

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f

myFirst :: (a -> d) -> (a,b,c) -> (d,b,c)
myFirst f (a,b,c) = (f a, b, c)

mySecond :: (b -> d) -> (a,b,c) -> (a,d,c)
mySecond f (a,b,c) = (a, f b, c)

third :: (c -> d) -> (a,b,c) -> (a,b,d)
third f (a,b,c) = (a, b, f c)

initializePlayers :: [String] -> PlayersStatus
initializePlayers s = (\ (a,b) -> (a,b,True)) . both ((,0) . (read :: String -> Int) . (:[]) . last) $ (head s, s !! 1)

dice :: DeterministicDice
dice = (0,0)

initializeGame :: [String] -> Game
initializeGame ls = (initializePlayers ls,dice)

modReturnToOne :: Int -> Int -> Int
modReturnToOne m x = (max (x-1) 0 `mod` m) + 1

rollOnce :: DeterministicDice -> (Int,DeterministicDice)
rollOnce (val,timesRolled) = (newVal,(modReturnToOne 100 newVal, timesRolled+1))
  where
    newVal = val + 1

rollThreeTimes :: DeterministicDice -> (Int,DeterministicDice)
rollThreeTimes = helper 3 0
  where
    helper 0 acc dd = (acc,dd)
    helper n acc dd =
      let (newVal,newDd) = rollOnce dd
      in helper (n-1) (acc + newVal) newDd

playerOneIsInTurn :: PlayersStatus -> Bool
playerOneIsInTurn (_,_,b) = b

moveOneByOne :: Int -> Int -> Int
moveOneByOne 0 currentPos = currentPos
moveOneByOne steps currentPos = moveOneByOne (steps-1) (modReturnToOne 10 $ currentPos + 1)

moveSteps :: Int -> Player -> Player
moveSteps steps (pos,points) =
  let stoppedOn = moveOneByOne steps pos
  in (stoppedOn, points + stoppedOn)

switchPlayers :: PlayersStatus -> PlayersStatus
switchPlayers = third not

oneTurn :: Game -> Game
oneTurn (ps,dd) =
  let (steps,newDd) = rollThreeTimes dd
      newPlayersStatus = switchPlayers . applyToCurrentPlayer $ moveSteps steps
        where applyToCurrentPlayer = if playerOneIsInTurn ps then flip myFirst ps else flip mySecond ps
  in (newPlayersStatus, newDd)

hasEnoughPoints :: Player -> Bool
hasEnoughPoints (_,points) = points >= 1000

otherPlayerWon :: PlayersStatus -> Bool
otherPlayerWon ps =  hasEnoughPoints . getOtherPlayer $ ps
  where getOtherPlayer = if playerOneIsInTurn ps then \ (_,p,_) -> p else \ (p,_,_) -> p

getScore :: Player -> Int
getScore = snd

getCurrentPlayerScore :: Game -> Int
getCurrentPlayerScore (ps,_) = if playerOneIsInTurn ps then getScore . (\ (p,_,_) -> p) $ ps else getScore . (\ (_,p,_) -> p) $ ps

getRollCount :: DeterministicDice -> Int
getRollCount = snd

getTotalDiceRolls :: Game -> Int
getTotalDiceRolls (_,dd) = getRollCount dd

endSituationScore :: Game -> Int
endSituationScore g@(ps,_) =
  if otherPlayerWon ps
    then getTotalDiceRolls g * getCurrentPlayerScore g
    else endSituationScore . oneTurn $ g

firstPart :: IO Int
firstPart = endSituationScore . initializeGame <$> input

splitByPoints :: Paths -> (Int,Paths)
splitByPoints [] = (0,[])
splitByPoints ((p,n):rest) =
  if snd p >= 21 then (\ (w,paths) -> (w+n,paths)) $ splitByPoints rest else (\ (w,lp) -> (w,(p,n):lp)) $ splitByPoints rest

afterOneMove :: Path -> (Int,Paths)
afterOneMove (p,count) =
  let allSteps = map (\ l -> (head l, count * length l)) . group . sort $ [ (\ (a,b,c) -> a+b+c) (x,y,z) | x <- [1..3], y <- [1..3], z <- [1..3]]
      allOutComes = map (\ (x,n) -> (moveSteps x p,n)) allSteps
  in splitByPoints allOutComes

afterAllMoves :: Paths -> (Int,Paths)
afterAllMoves ps =
  let allMoves = map afterOneMove ps
      wins = sum . map fst $ allMoves
      paths = foldl1 combinePaths . map snd $ allMoves
  in (wins,paths)

combineOne :: Paths -> Path -> Paths
combineOne [] p = [p]
combineOne (p:ps) path = if fst p == fst path then (fst p, snd p + snd path) : ps else p : combineOne ps path

combinePaths :: Paths -> Paths -> Paths
combinePaths = foldl combineOne

countBranches :: Paths -> Int
countBranches = sum . map snd

giveNextRoundOnePlayer :: (Int,Paths) -> (Int,Paths)
giveNextRoundOnePlayer (_,p) = afterAllMoves p

allSplitsOnePlayer :: (Int,Paths) -> [(Int,Paths)]
allSplitsOnePlayer input =
  let (wins,paths) = giveNextRoundOnePlayer input
  in if null paths then [input,(wins,paths)] else input : allSplitsOnePlayer (wins,paths)

splitsFromStartPlayer :: Player -> [(Int,Paths)]
splitsFromStartPlayer p = allSplitsOnePlayer (0,[(p,1)])

nextMoveSplits :: ((Int,Paths),(Int,Paths)) -> [(Int,Paths)] -> ((Int,Paths),(Int,Paths))
nextMoveSplits (cspl1,_) splits = (head splits,cspl1)

calculateScore :: ((Int,Paths),(Int,Paths)) -> [(Int,Paths)] -> [(Int,Paths)] -> Int -> Int -> (Int,Int)
calculateScore currentSplits splits1 splits2 wins1 wins2 =
  let openBranches = countBranches . snd . snd $ currentSplits
      newWins = (fst . fst $ currentSplits) * openBranches + wins1
  in if null . snd . fst $ currentSplits
       then (newWins,wins2)
       else let newSplits = nextMoveSplits currentSplits splits2 in calculateScore newSplits (tail splits2) splits1 wins2 newWins

scoreFromStartingPlayers :: Player -> Player -> (Int,Int)
scoreFromStartingPlayers p1 p2 =
  let (fs1:splits1) = splitsFromStartPlayer p1
      (fs2:splits2) = splitsFromStartPlayer p2
  in calculateScore (fs2,fs1) splits2 splits1 0 0

secondPart :: IO Int
secondPart = uncurry max . uncurry scoreFromStartingPlayers . (\ (a,b,c) -> (a,b)) . initializePlayers <$> input
