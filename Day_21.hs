{-# LANGUAGE TupleSections #-}

import Data.Bifunctor(bimap)
import Data.List(group,sort)
import Data.Maybe(fromMaybe)

type PlayersStatus = (Player,Player,Bool) -- player 1, player 2, player 1 is in turn
type Player = (Int,Int) -- position, points

type DeterministicDice = (Int,Int) -- value, times rolled

type Game = (PlayersStatus, DeterministicDice)

input :: IO [String]
input = lines <$> readFile "day_21_input.txt"

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f

first :: (a -> d) -> (a,b,c) -> (d,b,c)
first f (a,b,c) = (f a, b, c)

second :: (b -> d) -> (a,b,c) -> (a,d,c)
second f (a,b,c) = (a, f b, c)

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
        where applyToCurrentPlayer = if playerOneIsInTurn ps then flip first ps else flip second ps
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

type FrequencyList = [(Int,Int)] -- number of turns to reach 21 [in this case], amount of such numbers

combineOne :: FrequencyList -> (Int,Int) -> FrequencyList
combineOne [] val = [val]
combineOne ((val,count):rest) (x,n) = if x == val then (val,count + n) : rest else (val,count) : combineOne rest (x,n)

combineFrequencyLists :: FrequencyList -> FrequencyList -> FrequencyList
combineFrequencyLists fl1 fl2 = foldl combineOne fl2 fl1

splitByPoints :: [(Player,Int)] -> (Int,[(Player,Int)])
splitByPoints [] = (0,[])
splitByPoints ((p,n):rest) =
  if snd p >= 21 then (\ (w,lp) -> (w+n,lp)) $ splitByPoints rest else (\ (w,lp) -> (w,(p,n):lp)) $ splitByPoints rest

afterOneMove :: Player -> (Int,[(Player,Int)])
afterOneMove p =
  let allSteps = map (\ l -> (head l, length l)) . group . sort $ [ (\ (a,b,c) -> a+b+c) (x,y,z) | x <- [1..3], y <- [1..3], z <- [1..3]]
      allOutComes = map (\ (x,n) -> (moveSteps x p,n)) allSteps
  in splitByPoints allOutComes

multiplyFL :: Int -> FrequencyList -> FrequencyList
multiplyFL n = map (\ (x,m) -> (x,m*n))

addStepsToFL :: Int -> FrequencyList -> FrequencyList
addStepsToFL n = map (\ (x,m) -> (x+n,m))

getFrequencyList :: Player -> FrequencyList
getFrequencyList = helper 0
  where
    helper n p =
      let (wins,stillGoing) = afterOneMove p
          currentFL = [(n+1,wins) | wins /= 0]
      in if null stillGoing
           then currentFL
           else combineFrequencyLists currentFL $ foldl1 combineFrequencyLists $ map (\ (p,m) -> multiplyFL m . addStepsToFL (n+1) $ getFrequencyList p) stillGoing

countNumberOfPaths :: FrequencyList -> Int
countNumberOfPaths = foldr ((+) . snd) 0

winsTotal :: (FrequencyList,FrequencyList,Bool) -> Int -> (Int,Int) -> (Int,Int)
winsTotal (fl1,fl2,fpsTurn) openPaths winsSoFar =
  if openPaths == 0 then winsSoFar else
  let currentList = if fpsTurn then fl1 else fl2
      endingNow = fromMaybe 0 (lookup 1 currentList)
      wins = endingNow * openPaths
      newWins = if fpsTurn then (fst winsSoFar + wins, snd winsSoFar) else (fst winsSoFar, snd winsSoFar + wins)
      remainingList = filter (\ el -> fst el > 0) (map (\ (x,n) -> (x-1,n)) currentList)
      openPossibilities = countNumberOfPaths remainingList
      newOpenPaths = openPaths * openPossibilities
      modifiedLists = if fpsTurn then (remainingList,fl2,not fpsTurn) else (fl1,remainingList,not fpsTurn)
  in winsTotal modifiedLists newOpenPaths newWins

calculateHigherWinScore :: PlayersStatus -> Int
calculateHigherWinScore = uncurry max . (\ ffb -> winsTotal ffb 1 (0,0)) . (\ (a,b,c) -> (getFrequencyList a, getFrequencyList b, c))

secondPart :: IO Int
secondPart = calculateHigherWinScore . initializePlayers <$> input
