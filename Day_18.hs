import Data.Bifunctor(bimap,first)
import Data.Char(intToDigit)
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe)

data SnailfishNumberElement a = N a | P (SnailfishNumberElement a,SnailfishNumberElement a)
type SnailfishNumber = (SnailfishNumberElement Int,SnailfishNumberElement Int)
type PathToElement = String

instance Show a => Show (SnailfishNumberElement a) where
    show (N val) = " N " ++ show val
    show (P (val1,val2)) = " P (" ++ show val1 ++ "," ++ show val2 ++ ")"

instance Functor SnailfishNumberElement where
  fmap f (N val) = N (f val)
  fmap f (P (val1,val2)) = P (fmap f val1,fmap f val2)

input :: IO [SnailfishNumber]
input = map parseAsSnailfishNumber . lines <$> readFile "day_18_input.txt"

bracketCount :: String -> Int
bracketCount [] = 0
bracketCount (c:cs)
  | c == '[' = 1 + bracketCount cs
  | c == ']' = -1 + bracketCount cs
  | otherwise = bracketCount cs

concatWith :: a -> [[a]] -> [a]
concatWith _ [] = []
concatWith _ [x] = x
concatWith inBetween (el:rest) = el ++ [inBetween] ++ concatWith inBetween rest

reconstructParts :: ((String,Int) -> Bool) -> [(String,Int)] -> (String,String)
reconstructParts p [] = undefined
reconstructParts p (el:rest) = if p el then (fst el, concatWith ',' (map fst rest)) else first ((fst el ++ ",") ++ ) $ reconstructParts p rest

reconstructByBrackets :: [String] -> (String,String)
reconstructByBrackets s =
  if length s == 2
    then (\ [x,y] -> (x,y)) s
    else let cumulativeBracketSums = scanl1 (+) $ map bracketCount s
             splitsWithBracketCounts = zip s cumulativeBracketSums
         in reconstructParts ((== 0) . snd) splitsWithBracketCounts

splitInTwo :: String -> (String,String)
splitInTwo s =
  let multipleSplits = splitOn "," (tail . init $ s)
  in reconstructByBrackets multipleSplits

both :: (a -> b) -> (a,a) -> (b,b)
both f = bimap f f

isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

parseAsSnailfishNumberElement :: String -> SnailfishNumberElement Int
parseAsSnailfishNumberElement s = if isDigit . head $ s then N $ (read :: String -> Int) [head s] else P $ both parseAsSnailfishNumberElement (splitInTwo s)

parseAsSnailfishNumber :: String -> SnailfishNumber
parseAsSnailfishNumber s = both parseAsSnailfishNumberElement (splitInTwo s)

addSnailfishNumbers :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
addSnailfishNumbers (a1,a2) (b1,b2) = reduceSnailfishNumber (P (a1,a2), P (b1,b2))

reduceSnailfishNumber :: SnailfishNumber -> SnailfishNumber
reduceSnailfishNumber sfn
  | needsToExplode sfn = reduceSnailfishNumber $ explodeSfn sfn
  | needsToSplit sfn = reduceSnailfishNumber $ splitSfn sfn
  | otherwise = sfn

intAsString :: Int -> String
intAsString n =
  if n >= 10
    then intAsString (n `div` 10) ++ intAsString (n `mod` 10)
    else [intToDigit n]

sfnelAsString :: SnailfishNumberElement Int -> String
sfnelAsString (N val) = intAsString val
sfnelAsString (P (val1,val2)) = "[" ++ sfnelAsString val1 ++ "," ++ sfnelAsString val2 ++ "]"

sfnAsString :: SnailfishNumber -> String
sfnAsString (val1,val2) = "[" ++ sfnelAsString val1 ++ "," ++ sfnelAsString val2 ++ "]"

needsToExplode :: SnailfishNumber -> Bool
needsToExplode = elem 5 . scanl1 (+) . map bracketCount . splitOn "," . sfnAsString

pickJust :: (Maybe a, Maybe a) -> Maybe a
pickJust (mval1,mval2) =
  case mval1 of
    orig@(Just _) -> orig
    Nothing -> mval2

giveExploding :: SnailfishNumber -> Maybe (SnailfishNumberElement Int)
giveExploding = pickJust . both (helper 3)
  where
    helper 0 (N _) = Nothing
    helper 0 val = Just val
    helper n (N _) = Nothing
    helper n (P (val1,val2)) = pickJust . both (helper (n-1)) $ (val1,val2)

givePathToExploding :: SnailfishNumber -> Maybe PathToElement
givePathToExploding (l,r) = reverse <$> pickJust (helper 3 "L" l, helper 3 "R" r)
  where
    helper 0 acc (N _) = Nothing
    helper 0 acc val = Just acc
    helper n acc (N _) = Nothing
    helper n acc (P (val1,val2)) = pickJust (helper (n-1) ('L':acc) val1, helper (n-1) ('R':acc) val2)

followPathElement :: PathToElement -> SnailfishNumberElement Int -> Maybe (SnailfishNumberElement Int)
followPathElement [] sfnel = Just sfnel
followPathElement (choice:rest) (N _) = Nothing
followPathElement (choice:rest) (P (val1,val2)) = if choice == 'L' then followPathElement rest val1 else followPathElement rest val2

followPath :: PathToElement -> SnailfishNumber -> Maybe (SnailfishNumberElement Int)
followPath [] sfn = Nothing
followPath (choice:rest) (l,r) = if choice == 'L' then followPathElement rest l else followPathElement rest r

pathToRegNumInGivenDirection :: Char -> SnailfishNumberElement Int -> PathToElement
pathToRegNumInGivenDirection c sfnel =
  case sfnel of
    (N _) -> []
    (P (lval,rval)) -> c : pathToRegNumInGivenDirection c (if c == 'L' then lval else rval)

continuePathToRegNum :: Char -> PathToElement -> SnailfishNumber -> PathToElement
continuePathToRegNum c pte sfn =
  let msfnel = followPath pte sfn
      flippedDirection = if c == 'L' then 'R' else 'L'
      rightOrLeftmost = msfnel >>= return . pathToRegNumInGivenDirection flippedDirection
  in maybe [] (pte ++) rightOrLeftmost

givePathToNextRegNumInGivenDirection :: Char -> PathToElement -> SnailfishNumber -> PathToElement
givePathToNextRegNumInGivenDirection c pte sfn = maybe [] (flip (continuePathToRegNum c) sfn . reverse) (helper c sfn (reverse pte))
  where
    helper c sfn [] = Nothing
    helper c sfn (choice:rest) = if choice == c then helper c sfn rest else Just $ c:rest

insertBetween :: b -> (a,c) -> (a,b,c)
insertBetween b (a,c) = (a,b,c)

pathsToNumsParticipatingInExplosion :: SnailfishNumber -> (PathToElement,PathToElement,PathToElement)
pathsToNumsParticipatingInExplosion sfn =
  let exploding = fromMaybe [] $ givePathToExploding sfn
  in insertBetween exploding $ both (\ c -> givePathToNextRegNumInGivenDirection c exploding sfn) ('L','R')

updateByPathElement :: PathToElement -> (SnailfishNumberElement Int -> SnailfishNumberElement Int) -> SnailfishNumberElement Int -> SnailfishNumberElement Int
updateByPathElement [] f sfnel = f sfnel
updateByPathElement (choice:rest) f orig@(N _) = orig
updateByPathElement (choice:rest) f (P (val1,val2)) = if choice == 'L' then P (updateByPathElement rest f val1,val2) else P (val1,updateByPathElement rest f val2)

updateByPath :: PathToElement -> (SnailfishNumberElement Int -> SnailfishNumberElement Int) -> SnailfishNumber -> SnailfishNumber
updateByPath [] f orig = orig
updateByPath (choice:rest) f (val1,val2) = if choice == 'L' then (updateByPathElement rest f val1,val2) else (val1,updateByPathElement rest f val2)

extractPair :: SnailfishNumberElement Int -> (Int,Int)
extractPair sfnel =
  case sfnel of
    P (N val1, N val2) -> (val1,val2)
    _ -> (0,0)

giveExplodingNums :: PathToElement -> SnailfishNumber -> (Int,Int)
giveExplodingNums pte sfn = extractPair . fromMaybe (P (N 0, N 0)) $ followPath pte sfn

explodeSfn :: SnailfishNumber -> SnailfishNumber
explodeSfn sfn =
  let (leftPath, explodingPath, rightPath) = pathsToNumsParticipatingInExplosion sfn
      (leftExploding,rightExploding) = giveExplodingNums explodingPath sfn
  in updateByPath leftPath (fmap (+leftExploding)) (updateByPath explodingPath (const (N 0)) (updateByPath rightPath (fmap (+rightExploding)) sfn))

hasRegularNumberGreaterThanNine :: SnailfishNumberElement Int -> Bool
hasRegularNumberGreaterThanNine (N val) = val > 9
hasRegularNumberGreaterThanNine (P (val1,val2)) = hasRegularNumberGreaterThanNine val1 || hasRegularNumberGreaterThanNine val2

needsToSplit :: SnailfishNumber -> Bool
needsToSplit = uncurry (||) . both hasRegularNumberGreaterThanNine

pathToSplittingElement :: SnailfishNumberElement Int -> Maybe PathToElement
pathToSplittingElement = fmap reverse . helper []
  where
    helper :: PathToElement -> SnailfishNumberElement Int -> Maybe PathToElement
    helper acc (N val) = if val > 9 then Just acc else Nothing
    helper acc (P (val1,val2)) = pickJust (helper ('L':acc) val1,helper ('R':acc) val2)

pathToSplitting :: SnailfishNumber -> PathToElement
pathToSplitting = fromMaybe [] . pickJust . bimap (fmap ('L':)) (fmap ('R':)) . both pathToSplittingElement

splitNumber :: SnailfishNumberElement Int -> SnailfishNumberElement Int
splitNumber (N val) =
  let decimalNumber = fromIntegral val / 2
      lval = floor decimalNumber
      rval = ceiling decimalNumber
  in P (N lval,N rval)
splitNumber (P _) = P (N 0,N 0)

splitSfn :: SnailfishNumber -> SnailfishNumber
splitSfn sfn =
  let splitPath = pathToSplitting sfn
  in updateByPath splitPath splitNumber sfn

magnitudeElement :: SnailfishNumberElement Int -> Int
magnitudeElement (N val) = val
magnitudeElement (P (val1,val2)) = 3 * magnitudeElement val1 + 2 * magnitudeElement val2

magnitude :: SnailfishNumber -> Int
magnitude = uncurry (+) . bimap ((3*) . magnitudeElement) ((2*) . magnitudeElement)

firstPart :: IO Int
firstPart = magnitude . foldl1 addSnailfishNumbers <$> input

testInputs :: [String]
testInputs = ["[1,2]", "[[1,2],3]", "[9,[8,7]]", "[[1,9],[8,5]]", "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]", "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]", "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"]

addingExample :: [String]
addingExample =
  ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
  ,"[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
  ,"[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
  ,"[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
  ,"[7,[5,[[3,8],[1,4]]]]"
  ,"[[2,[2,2]],[8,[8,1]]]"
  ,"[2,9]"
  ,"[1,[[[9,3],9],[[9,0],[0,7]]]]"
  ,"[[[5,[7,4]],7],1]"
  ,"[[[[4,2],2],6],[8,7]]"]
