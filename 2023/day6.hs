input :: IO [String]
input = lines <$> readFile "day6_input.txt"

parseTimes :: [String] -> [Int]
parseTimes = map read . words . drop 1 . dropWhile (/= ':') . (!! 0)

parseDistances :: [String] -> [Int]
parseDistances = map read . words . drop 1 . dropWhile (/= ':') . (!! 1)

raceRecords :: [String] -> [(Int,Int)]
raceRecords inp =
  let times = parseTimes inp
      distances = parseDistances inp
  in zip times distances

distanceWithHold :: Int -> Int -> Int
distanceWithHold raceTime holdTime = holdTime * (raceTime - holdTime)

waysOfBeatingRecord :: (Int,Int) -> Int
waysOfBeatingRecord (t,d) = length . filter (> d) . map (distanceWithHold t) $ [0..t]

firstPuzzle :: IO Int
firstPuzzle = product . map waysOfBeatingRecord . raceRecords <$> input

parseOneTime :: [String] -> Int
parseOneTime = read . concat . words . drop 1 . dropWhile (/= ':') . (!! 0)

parseOneDistance :: [String] -> Int
parseOneDistance = read . concat . words . drop 1 . dropWhile (/= ':') . (!! 1)

oneRaceRecord :: [String] -> (Int,Int)
oneRaceRecord inp =
  let time = parseOneTime inp
      distance = parseOneDistance inp
  in (time, distance)

secondPuzzle :: IO Int
secondPuzzle = waysOfBeatingRecord . oneRaceRecord <$> input

