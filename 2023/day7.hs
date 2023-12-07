import Data.List (group, sort, sortBy)

input :: IO [String]
input = lines <$> readFile "day7_input.txt"

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind 
  deriving (Eq, Ord, Show)

data Hand = Hand {handType :: HandType, cards :: String}
  deriving (Eq)

instance Show Hand where
  show h = show (handType h) ++ " " ++ cards h

cardsInOrder :: [Char]
cardsInOrder = "23456789TJQKA"

singleCardOrder :: Char -> Char -> Ordering
singleCardOrder a b =
  let numberedCards = zip cardsInOrder [1..]
      aNumber = snd . head . filter ((== a) . fst) $ numberedCards
      bNumber = snd . head . filter ((== b) . fst) $ numberedCards
  in compare aNumber bNumber

cardsOrder :: [Char] -> [Char] -> Ordering
cardsOrder [] [] = EQ
cardsOrder (x:xs) (y:ys) =
  let mostSignificantOrder = singleCardOrder x y
  in if (mostSignificantOrder == EQ)
       then cardsOrder xs ys
       else mostSignificantOrder

instance Ord Hand where
  h1 <= h2 =
    if (handType h1 == handType h2)
      then cardsOrderWithJokers (cards h1) (cards h2) `elem` [LT, EQ]
      else handType h1 <= handType h2

handTypeFromProfile :: [Int] -> HandType
handTypeFromProfile ls
  | 5 `elem` ls = FiveOfAKind
  | 4 `elem` ls = FourOfAKind
  | 3 `elem` ls = if (2 `elem` ls) then FullHouse else ThreeOfAKind
  | (length . filter (== 2) $ ls) == 2 = TwoPair
  | 2 `elem` ls = OnePair
  | otherwise = HighCard

readHand :: String -> Hand
readHand s = Hand (handTypeFromProfile (map length . group . sort $ s)) s

readHandAndBid :: String -> (Hand,Int)
readHandAndBid s = (readHand (splitString !! 0), read (splitString !! 1))
  where
    splitString = words s

giveTotalWinnings :: [String] -> Int
giveTotalWinnings allLines =
  let handBidsInOrder = sortBy (\ (a,_) (b,_) -> compare a b) . map readHandAndBid $ allLines
  in sum . map (\ ((_,bid),rank) -> bid * rank) $ zip handBidsInOrder [1..]

firstPuzzle :: IO Int
firstPuzzle = giveTotalWinnings <$> input

cardsInOrderWithJokers :: [Char]
cardsInOrderWithJokers = "J23456789TQKA"

singleCardOrderWithJokers :: Char -> Char -> Ordering
singleCardOrderWithJokers a b =
  let numberedCards = zip cardsInOrderWithJokers [1..]
      aNumber = snd . head . filter ((== a) . fst) $ numberedCards
      bNumber = snd . head . filter ((== b) . fst) $ numberedCards
  in compare aNumber bNumber

cardsOrderWithJokers :: [Char] -> [Char] -> Ordering
cardsOrderWithJokers [] [] = EQ
cardsOrderWithJokers (x:xs) (y:ys) =
  let mostSignificantOrder = singleCardOrderWithJokers x y
  in if (mostSignificantOrder == EQ)
       then cardsOrderWithJokers xs ys
       else mostSignificantOrder

giveBestHandTypeWithTwoJokers :: [Int] -> HandType
giveBestHandTypeWithTwoJokers profile
  | 3 `elem` profile = FiveOfAKind
  | 2 `elem` profile = FourOfAKind
  | otherwise = ThreeOfAKind

giveBestHandTypeWithOneJoker :: [Int] -> HandType
giveBestHandTypeWithOneJoker profile
  | 4 `elem` profile = FiveOfAKind
  | 3 `elem` profile = FourOfAKind
  | 2 `elem` profile = if (length (filter (== 2) profile) == 2) then FullHouse else ThreeOfAKind
  | otherwise = OnePair

giveBestHandTypeWithJokers :: [Int] -> Int -> HandType
giveBestHandTypeWithJokers profile jokerCount
  | jokerCount `elem` [4,5] = FiveOfAKind
  | jokerCount == 3 = if (length profile == 1) then FiveOfAKind else FourOfAKind
  | jokerCount == 2 = giveBestHandTypeWithTwoJokers profile
  | jokerCount == 1 = giveBestHandTypeWithOneJoker profile
  | otherwise = handTypeFromProfile profile

handTypeFromProfileWithJokers :: [(Char,Int)] -> HandType
handTypeFromProfileWithJokers profile =
  let jokerEntry = filter (\ p -> fst p == 'J') profile
      jokerCount = if (length jokerEntry > 0) then snd . head $ jokerEntry else 0
      profileWithoutJokers = map snd . filter ((/= 'J' ) . fst) $ profile
  in giveBestHandTypeWithJokers profileWithoutJokers jokerCount

readHandWithJokers :: String -> Hand
readHandWithJokers s =
  let handType = handTypeFromProfileWithJokers . map (\ cs -> (head cs, length cs)) . group . sort $ s
  in Hand handType s

readHandAndBidWithJokers :: String -> (Hand,Int)
readHandAndBidWithJokers s = (readHandWithJokers (splitString !! 0), read (splitString !! 1))
  where
    splitString = words s

giveTotalWinningsWithJokers :: [String] -> Int
giveTotalWinningsWithJokers allLines =
  let handBidsInOrder = sortBy (\ (a,_) (b,_) -> compare a b) . map readHandAndBidWithJokers $ allLines
  in sum . map (\ ((_,bid),rank) -> bid * rank) $ zip handBidsInOrder [1..]

secondPuzzle :: IO Int
secondPuzzle = giveTotalWinningsWithJokers <$> input

testInput :: [String]
testInput = ["32T3K 765",
  "T55J5 684",
  "KK677 28",
  "KTJJT 220",
  "QQQJA 483"]

