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

type ResourceRange = (String,Integer,Integer)

getSeedRanges :: [String] -> [ResourceRange]
getSeedRanges = convertToSeedRanges . map read . words . drop 1 . dropWhile (/= ':') . (!! 0)
  where
    convertToSeedRanges :: [Integer] -> [ResourceRange]
    convertToSeedRanges [] = []
    convertToSeedRanges (start:range:rest) = ("seed", start, range) : convertToSeedRanges rest

biggestSubrangeWithRule :: String -> (Integer,Integer,Integer) -> ResourceRange -> ResourceRange
biggestSubrangeWithRule resType (dest, ruleStart, ruleRange) (_, start, range) =
  let reducedRuleRange = ruleRange - (start - ruleStart)
      tightestRange = min range reducedRuleRange
      newStart = start + (dest - ruleStart)
  in (resType, newStart, tightestRange)

biggestSubrangeNoRule :: String -> Map -> ResourceRange -> ResourceRange
biggestSubrangeNoRule resType m (_, start, range) =
  let allBiggerRuleStarts = filter (> start) . map (\ (_,ruleStart,_) -> ruleStart) $ ranges m
      tightestRange = if (length allBiggerRuleStarts > 0) then min range (minimum allBiggerRuleStarts - start) else range
  in (resType, start, tightestRange)

reduceRange :: ResourceRange -> ResourceRange -> ResourceRange
reduceRange (_, _, subRange) (resType, start, range) =
  let newRange = max 0 (range - subRange)
      newStart = start + subRange
  in (resType, newStart, newRange)

splitAndConvertRangeWithRightMap :: Map -> ResourceRange -> [ResourceRange]
splitAndConvertRangeWithRightMap _ (_, _, 0) = []
splitAndConvertRangeWithRightMap m rr@(_, start, range) =
  let matchingRangeList = filter (\ (_,s,r) -> isInRange s r start) $ ranges m
      convertedType = snd $ mapType m
      subRange = if (length matchingRangeList > 0)
                   then biggestSubrangeWithRule convertedType (head matchingRangeList) rr
                   else biggestSubrangeNoRule convertedType m rr
      remainingRange = reduceRange subRange rr
  in subRange : splitAndConvertRangeWithRightMap m remainingRange

convertOneResourceRange :: [Map] -> ResourceRange -> [ResourceRange]
convertOneResourceRange allMaps rr@(currentType, _, _) =
  let rightMap = head . filter (\ m -> fst (mapType m) == currentType) $ allMaps
  in splitAndConvertRangeWithRightMap rightMap rr

convertAllResourceRanges :: [Map] -> [ResourceRange] -> [ResourceRange]
convertAllResourceRanges m rrs = rrs >>= convertOneResourceRange m

getMinimumLocationFromRanges :: [Map] -> ResourceRange -> Integer
getMinimumLocationFromRanges _ ("location", start, _) = start
getMinimumLocationFromRanges m rr = minimum . map (getMinimumLocationFromRanges m ) $ convertOneResourceRange m rr

minimumLocationFromInput :: [String] -> Integer
minimumLocationFromInput inp =
  let allMaps = getAllMaps inp
      seedRanges = getSeedRanges inp
  in minimum . map (getMinimumLocationFromRanges allMaps) $ seedRanges

secondPuzzle :: IO Integer
secondPuzzle = minimumLocationFromInput <$> input

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

