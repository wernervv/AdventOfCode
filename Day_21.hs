{-# LANGUAGE TupleSections #-}

import Data.Bifunctor(bimap)

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
