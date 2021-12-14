import Data.List.Split
import Data.List(transpose)

input :: IO [String]
input = lines <$> readFile "day_4_input.txt"

drawnNumbers :: IO [Int]
drawnNumbers = map (read :: String -> Int) . splitOn "," . head <$> input

boards :: IO [[[Int]]]
boards = map (map giveAsNumbers . filter (/= "")) . chunksOf 6 . tail <$> input
  where
    giveAsNumbers = map (read :: String -> Int) . words

initBingo :: IO [[[(Int, Bool)]]]
initBingo = map (map initRow) <$> boards

initRow :: [Int] -> [(Int, Bool)]
initRow = map (flip (,) False)

isBingo :: [[(Int, Bool)]] -> Bool
isBingo board = any (all snd) board || any (all snd) (transpose board)

drawNumber :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
drawNumber n = map (map (markNumber n))

markNumber :: Int -> (Int, Bool) -> (Int, Bool)
markNumber n (val, isMarked) = if val == n then (val, True) else (val, isMarked)

winnerAndNumber :: IO ([[(Int,Bool)]],Int)
winnerAndNumber = helper <$> drawnNumbers <*> initBingo
  where
    helper [] boards = ([[]],0)
    helper (n:ns) boards = let marked = map (map (map (markNumber n))) boards
                               winnerList = filter isBingo marked
                           in if not $ null winnerList
                                then (head winnerList, n)
                                else helper ns marked

calculateScore :: ([[(Int,Bool)]],Int) -> Int
calculateScore (wb, n) = n * (sum . map fst . filter (not . snd) $ concat wb)

firstPart :: IO Int
firstPart = calculateScore <$> winnerAndNumber

lastToWinAndNumber :: [Int] -> [[[(Int,Bool)]]] -> ([[(Int,Bool)]],Int)
lastToWinAndNumber [] boards = (last boards,0)
lastToWinAndNumber (n:ns) boards =
  let markedBoards = map (map (map (markNumber n))) boards
      stillPlaying = filter (not . isBingo) markedBoards
      candidate    = last markedBoards
  in case length stillPlaying of
       0 -> (candidate, n)
       1 -> if isBingo (head stillPlaying) then (head stillPlaying, n) else lastToWinAndNumber ns stillPlaying
       _ -> lastToWinAndNumber ns stillPlaying

secondPart :: IO Int
secondPart = calculateScore <$> (lastToWinAndNumber <$> drawnNumbers <*> initBingo)
