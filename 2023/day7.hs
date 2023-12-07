import Data.List (group, sort, sortBy)

input :: IO [String]
input = lines <$> readFile "day7_input.txt"

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind 
  deriving (Eq, Ord)

data Hand = Hand {handType :: HandType, cards :: String}
  deriving (Eq)

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
      then cardsOrder (cards h1) (cards h2) `elem` [LT, EQ]
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

testInput :: [String]
testInput = ["32T3K 765",
  "T55J5 684",
  "KK677 28",
  "KTJJT 220",
  "QQQJA 483"]

