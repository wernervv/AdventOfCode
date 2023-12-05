input :: IO [String]
input = lines <$> readFile "day5_input.txt"

type Resource = (String, Integer)

getSeeds :: [String] -> [Resource]
getSeeds = map (\ n -> ("seed", n)) . map read . words . drop 1 . dropWhile (/= ':') . (!! 0)

data Map = Map { mapType :: (String,String), ranges :: [(Integer,Integer,Integer)]}

parseOneMap :: [String] -> Map
parseOneMap [] = Map ("","") []
parseOneMap (l:ls) =
  let sourceType = takeWhile (/= '-') l
      destinationType = takeWhile (/= ' ') . drop 1 . dropWhile (/= '-') . drop 1 . dropWhile (/= '-') $ l
  in Map (sourceType, destinationType) (map ((\ (x:y:z:[]) -> (x,y,z)) . (map read . words)) ls)

splitIntoMapStrings :: [String] -> [[String]]
splitIntoMapStrings s = go [] (drop 1 . dropWhile (/= "") $ s)
  where
    go allMaps [] = allMaps
    go allMaps s = go ((takeWhile (/= "") s) : allMaps) (drop 1 . dropWhile (/= "") $ s)

getAllMaps :: [String] -> [Map]
getAllMaps = map parseOneMap . splitIntoMapStrings

isInRange :: Integer -> Integer -> Integer -> Bool
isInRange start range val = val >= start && val < (start + range)

convertWithRightRange :: (Integer,Integer,Integer) -> Integer -> Integer
convertWithRightRange (dest, source, _) val = val + (dest - source)

convertWithRightMap :: Map -> Resource -> Resource
convertWithRightMap m res =
  let rightRangeList = filter (\ (_,s,r) -> isInRange s r (snd res)) (ranges m)
      typeAfterConversion = snd . mapType $ m
  in if (length rightRangeList > 0)
       then (typeAfterConversion, convertWithRightRange (head rightRangeList) (snd res))
       else (typeAfterConversion, snd res)

convertResource :: [Map] -> Resource -> Resource
convertResource allMaps res =
  let resourceType = fst res
      rightMap = head . filter (\ m -> (fst . mapType $ m) == resourceType) $ allMaps
  in convertWithRightMap rightMap res

getLocation :: [Map] -> Resource -> Integer
getLocation _ ("location", x) = x
getLocation allMaps res = getLocation allMaps (convertResource allMaps res)

getMinimumLocation :: [String] -> Integer
getMinimumLocation inp =
  let seeds = getSeeds inp
      allMaps = getAllMaps inp
  in minimum . map (getLocation allMaps) $ seeds

firstPuzzle :: IO Integer
firstPuzzle = getMinimumLocation <$> input

testInput :: [String]
testInput = ["seeds: 79 14 55 13",
  "",
  "seed-to-soil map:",
  "50 98 2",
  "52 50 48",
  "",
  "soil-to-fertilizer map:",
  "0 15 37",
  "37 52 2",
  "39 0 15",
  "",
  "fertilizer-to-water map:",
  "49 53 8",
  "0 11 42",
  "42 0 7",
  "57 7 4",
  "",
  "water-to-light map:",
  "88 18 7",
  "18 25 70",
  "",
  "light-to-temperature map:",
  "45 77 23",
  "81 45 19",
  "68 64 13",
  "",
  "temperature-to-humidity map:",
  "0 69 1",
  "1 0 69",
  "",
  "humidity-to-location map:",
  "60 56 37",
  "56 93 4"]

