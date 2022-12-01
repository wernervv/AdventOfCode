import Data.Bifunctor(first,bimap)
import Data.Char(digitToInt,ord)

type Bits = [Int]
type Hex = String
type Packet = Bits
type TypeID = Int
type Version = Int

data GeneralParsedPacket a = Sum a -- sum
                           | Prod a -- product
                           | Min a -- minimum
                           | Max a -- maximum
                           | LV a -- Literal Value
                           | Gt a -- greater than
                           | Lt a -- less than
                           | Eq a -- equal to
                           | Op [GeneralParsedPacket a] -- Operator

type ParsedPacket = GeneralParsedPacket Int

instance Show a => Show (GeneralParsedPacket a) where
    show (Sum val) = "Sum " ++ show val
    show (Prod val) = "Prod " ++ show val
    show (Min val) = "Min " ++ show val
    show (Max val) = "Max " ++ show val
    show (LV val) = "LV " ++ show val
    show (Gt val) = "Gt " ++ show val
    show (Lt val) = "Lt " ++ show val
    show (Eq val) = "Eq " ++ show val
    show (Op ls) = "Op " ++ show ls

instance Functor GeneralParsedPacket where
     
    fmap f (Sum val) = Sum $ f val
    fmap f (Prod val) = Prod $ f val
    fmap f (Min val) = Min $ f val
    fmap f (Max val) = Max $ f val
    fmap f (LV val) = LV $ f val
    fmap f (Gt val) = Gt $ f val
    fmap f (Lt val) = Lt $ f val
    fmap f (Eq val) = Eq $ f val
    fmap f (Op val) = Op $ map (fmap f) val

instance Applicative GeneralParsedPacket where
    pure = undefined
    Sum f <*> val = fmap f val
    Prod f <*> val = fmap f val
    Min f <*> val = fmap f val
    Max f <*> val = fmap f val
    LV f <*> val = fmap f val
    Gt f <*> val = fmap f val
    Lt f <*> val = fmap f val
    Eq f <*> val = fmap f val
    Op f <*> val = Op $ map (<*> val) f

getValue :: ParsedPacket -> [Int]
getValue (Sum val) = [val]
getValue (Prod val) = [val]
getValue (Min val) = [val]
getValue (Max val) = [val]
getValue (LV val) = [val]
getValue (Gt val) = [val]
getValue (Lt val) = [val]
getValue (Eq val) = [val]
getValue (Op vals) = concatMap getValue vals

input :: IO Hex
input = head . lines <$> readFile "day_16_input.txt"

prependToLengthWith :: Int -> a -> [a] -> [a]
prependToLengthWith n el ls = if length ls >= n then ls else prependToLengthWith n el (el:ls)

digitToFourBits :: Int -> Bits
digitToFourBits = prependToLengthWith 4 0 . reverse . helper
  where
    helper 0 = [0]
    helper 1 = [1]
    helper n =
      let current = n `mod` 2
      in current : helper ((n-current) `div` 2)

hexCharToBits :: Char -> Bits
hexCharToBits = digitToFourBits . (\ c -> if 'A' <= c && c <= 'F' then ord c - ord 'A' + 10 else digitToInt c)

hexToBits :: Hex -> Bits
hexToBits = concatMap hexCharToBits

bitsToInt :: Bits -> Int
bitsToInt = helper 0
  where
    helper acc [] = acc
    helper acc (b:bs) = helper (2*acc + b) bs

separateHeader :: Packet -> (Version,TypeID,Packet)
separateHeader p = (bitsToInt $ take 3 p, bitsToInt $ take 3 . drop 3 $ p, drop 6 p)

parseLiteralValue :: Packet -> (ParsedPacket,Int)
parseLiteralValue = first (LV . bitsToInt) . helper 6
  where
    helper n packet =
      let firstFive = take 5 packet
          rest = drop 5 packet
          numberPart = tail firstFive
          newN = n + 5
      in if head firstFive == 0
           then (numberPart, newN)
           else first (numberPart ++) $ helper newN rest

parseByTotalLength :: Int -> Packet -> ([ParsedPacket],Int)
parseByTotalLength = helper 0
  where
    helper acc n packet =
      let (parsed,lengthParsed) = parsePacket packet
          parsedSoFar = lengthParsed + acc
      in if parsedSoFar == n then ([parsed],parsedSoFar) else first (parsed :) $ helper parsedSoFar n (drop lengthParsed packet)

parseByTotalLengthSumVersionNumbers :: Int -> Int -> Packet -> ([ParsedPacket],Int,Int)
parseByTotalLengthSumVersionNumbers sumOfVersionNums = helper sumOfVersionNums 0
  where
    helper sumOfVersionNums acc n packet =
      let (parsed,lengthParsed,newSum) = parsePacketSumVersionNumbers sumOfVersionNums packet
          parsedSoFar = lengthParsed + acc
      in if parsedSoFar == n then ([parsed],parsedSoFar,newSum) else (\ (a,b,c) -> (parsed : a, b, c)) $ helper newSum parsedSoFar n (drop lengthParsed packet)

parseByPacketCount :: Int -> Packet -> ([ParsedPacket],Int)
parseByPacketCount = helper 0 0
  where
    helper count acc n packet =
      let (parsed,lengthParsed) = parsePacket packet
          newCount = count + 1
          parsedSoFar = lengthParsed + acc
      in if newCount == n then ([parsed],parsedSoFar) else first (parsed :) $ helper newCount parsedSoFar n (drop lengthParsed packet)

parseByPacketCountSumVersionNumbers :: Int -> Int -> Packet -> ([ParsedPacket],Int,Int)
parseByPacketCountSumVersionNumbers sumOfVersionNums = helper sumOfVersionNums 0 0
  where
    helper sumOfVersionNums count acc n packet =
      let (parsed,lengthParsed,newSum) = parsePacketSumVersionNumbers sumOfVersionNums packet
          newCount = count + 1
          parsedSoFar = lengthParsed + acc
      in if newCount == n then ([parsed],parsedSoFar,newSum) else (\ (a,b,c) -> (parsed : a, b, c)) $ helper newSum newCount parsedSoFar n (drop lengthParsed packet)

parseOperatorSumVersionNumbers :: Int -> Packet -> (ParsedPacket,Int,Int)
parseOperatorSumVersionNumbers sumOfVersionNums packet =
  let lengthTypeID = head packet
      rest = tail packet
      (numBitsCount, matchingFunc) = if lengthTypeID == 0 then (15, parseByTotalLengthSumVersionNumbers) else (11, parseByPacketCountSumVersionNumbers)
      num = bitsToInt . take numBitsCount $ rest
      n = 7 + numBitsCount
      payload = drop numBitsCount rest
  in (\ g f (a,b,c) -> (g a, f b, c)) Op (+n) $ matchingFunc sumOfVersionNums num payload

makePairIntoTripleWith :: c -> (a,b) -> (a,b,c)
makePairIntoTripleWith el (a,b) = (a,b,el)

parsePacketSumVersionNumbers :: Int -> Packet -> (ParsedPacket,Int,Int)
parsePacketSumVersionNumbers sumOfVersionNums packet =
  let (v,t,p) = separateHeader packet
      newSum = sumOfVersionNums + v
  in case t of
       4 -> makePairIntoTripleWith newSum $ parseLiteralValue p
       _ -> parseOperatorSumVersionNumbers newSum p

firstPart :: IO Int
firstPart = (\ (_,_,val) -> val) . parsePacketSumVersionNumbers 0 . hexToBits <$> input

parsePacket :: Packet -> (ParsedPacket,Int)
parsePacket packet =
  let (v,t,p) = separateHeader packet
  in case t of
       4 -> parseLiteralValue p
       _ -> parseOperator t p

parseOperator :: TypeID -> Packet -> (ParsedPacket,Int)
parseOperator typeID packet =
  let lengthTypeID = head packet
      rest = tail packet
      (numBitsCount, matchingFunc) = if lengthTypeID == 0 then (15, parseByTotalLength) else (11, parseByPacketCount)
      num = bitsToInt . take numBitsCount $ rest
      n = 7 + numBitsCount
      payload = drop numBitsCount rest
      parsedSubPackets = bimap Op (+n) $ matchingFunc num payload
  in summarizeResult typeID parsedSubPackets

summarizeResult :: TypeID -> (ParsedPacket,Int) -> (ParsedPacket,Int)
summarizeResult typeID (subPackets,sumOfVersionNums) =
  case typeID of
       0 -> sumSubPackets (subPackets,sumOfVersionNums)
       1 -> multiplySubPackets (subPackets,sumOfVersionNums)
       2 -> minimumOfSubPackets (subPackets,sumOfVersionNums)
       3 -> maximumOfSubPackets (subPackets,sumOfVersionNums)
       5 -> firstIsGreater (subPackets,sumOfVersionNums)
       6 -> firstIsSmaller (subPackets,sumOfVersionNums)
       7 -> areEqual (subPackets,sumOfVersionNums)
       _ -> (subPackets,sumOfVersionNums)

sumSubPackets :: (ParsedPacket,Int) -> (ParsedPacket,Int)
sumSubPackets (subPackets,sumOfVersionNums) =
  let sumOfSubPacketVals = sum $ getValue subPackets
  in (Sum sumOfSubPacketVals,sumOfVersionNums)

multiplySubPackets :: (ParsedPacket,Int) -> (ParsedPacket,Int)
multiplySubPackets (subPackets,sumOfVersionNums) =
  let productOfSubPacketVals = product $ getValue subPackets
  in (Prod productOfSubPacketVals,sumOfVersionNums)

minimumOfSubPackets :: (ParsedPacket,Int) -> (ParsedPacket,Int)
minimumOfSubPackets (subPackets,sumOfVersionNums) =
  let minimumOfSubPacketVals = minimum $ getValue subPackets
  in (Min minimumOfSubPacketVals,sumOfVersionNums)

maximumOfSubPackets :: (ParsedPacket,Int) -> (ParsedPacket,Int)
maximumOfSubPackets (subPackets,sumOfVersionNums) =
  let maximumOfSubPacketVals = maximum $ getValue subPackets
  in (Max maximumOfSubPacketVals,sumOfVersionNums)

firstIsGreater :: (ParsedPacket,Int) -> (ParsedPacket,Int)
firstIsGreater (subPackets,sumOfVersionNums) =
  let subPacketVals = getValue subPackets
      returnVal = if head subPacketVals > subPacketVals !! 1 then Gt 1 else Gt 0
  in (returnVal,sumOfVersionNums)

firstIsSmaller :: (ParsedPacket,Int) -> (ParsedPacket,Int)
firstIsSmaller (subPackets,sumOfVersionNums) =
  let subPacketVals = getValue subPackets
      returnVal = if head subPacketVals < subPacketVals !! 1 then Lt 1 else Lt 0
  in (returnVal,sumOfVersionNums)

areEqual :: (ParsedPacket,Int) -> (ParsedPacket,Int)
areEqual (subPackets,sumOfVersionNums) =
  let subPacketVals = getValue subPackets
      returnVal = if head subPacketVals == subPacketVals !! 1 then Eq 1 else Eq 0
  in (returnVal,sumOfVersionNums)

secondPart :: IO Int
secondPart = head . getValue . fst . parsePacket . hexToBits <$> input
