import Data.Bifunctor(first,bimap)
import Data.Char(digitToInt,ord)

type Bits = [Int]
type Hex = String
type Packet = Bits
type TypeID = Int
type Version = Int

data ParsedPacket = LV Int | Op [ParsedPacket] -- Literal Value | Operator

instance Show ParsedPacket where
    show (LV val) = "LV " ++ show val
    show (Op ls) = "Op " ++ show ls

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

parseOperator :: Packet -> (ParsedPacket,Int)
parseOperator packet =
  let lengthTypeID = head packet
      rest = tail packet
      (numBitsCount, matchingFunc) = if lengthTypeID == 0 then (15, parseByTotalLength) else (11, parseByPacketCount)
      num = bitsToInt . take numBitsCount $ rest
      n = 7 + numBitsCount
      payload = drop numBitsCount rest
  in bimap Op (+n) $ matchingFunc num payload

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

parsePacket :: Packet -> (ParsedPacket,Int)
parsePacket packet =
  let (v,t,p) = separateHeader packet
  in case t of
       4 -> parseLiteralValue p
       _ -> parseOperator p

parsePacketSumVersionNumbers :: Int -> Packet -> (ParsedPacket,Int,Int)
parsePacketSumVersionNumbers sumOfVersionNums packet =
  let (v,t,p) = separateHeader packet
      newSum = sumOfVersionNums + v
  in case t of
       4 -> makePairIntoTripleWith newSum $ parseLiteralValue p
       _ -> parseOperatorSumVersionNumbers newSum p

firstPart :: IO Int
firstPart = (\ (_,_,val) -> val) . parsePacketSumVersionNumbers 0 . hexToBits <$> input
