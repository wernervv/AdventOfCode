import Data.Bifunctor(bimap,first)
import Data.Char(intToDigit)
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe)

data SnailfishNumberElement = N Int | P (SnailfishNumberElement,SnailfishNumberElement)
type SnailfishNumber = (SnailfishNumberElement,SnailfishNumberElement)
type PathToElement = String

instance Show SnailfishNumberElement where
    show (N val) = " N " ++ show val
    show (P (val1,val2)) = " P (" ++ show val1 ++ "," ++ show val2 ++ ")"

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

parseAsSnailfishNumberElement :: String -> SnailfishNumberElement
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

sfnelAsString :: SnailfishNumberElement -> String
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

giveExploding :: SnailfishNumber -> Maybe SnailfishNumberElement
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

followPathElement :: PathToElement -> SnailfishNumberElement -> Maybe SnailfishNumberElement
followPathElement [] sfnel = Just sfnel
followPathElement (choice:rest) (N _) = Nothing
followPathElement (choice:rest) (P (val1,val2)) = if choice == 'L' then followPathElement rest val1 else followPathElement rest val2

followPath :: PathToElement -> SnailfishNumber -> Maybe SnailfishNumberElement
followPath [] sfn = Nothing
followPath (choice:rest) (l,r) = if choice == 'L' then followPathElement rest l else followPathElement rest r

rightmostRegNumPathEnding :: SnailfishNumberElement -> PathToElement
rightmostRegNumPathEnding sfnel =
  case sfnel of
    (N _) -> []
    (P (_,rval)) -> 'R' : rightmostRegNumPathEnding rval

giveRightmostRegularBranch :: PathToElement -> SnailfishNumber -> PathToElement
giveRightmostRegularBranch pte sfn =
  let msfnel = followPath pte sfn
      rightMost = msfnel >>= return . rightmostRegNumPathEnding
  in maybe [] (pte ++) rightMost

givePathToFirstRegularLeft :: PathToElement -> SnailfishNumber -> PathToElement
givePathToFirstRegularLeft pte sfn = maybe [] ((`giveRightmostRegularBranch` sfn) . reverse) (helper [] sfn (reverse pte))
  where
    helper acc sfn [] = Nothing
    helper acc sfn (choice:rest) = if choice == 'L' then helper acc sfn rest else Just $ 'L':rest

pathToFirstToBeAdded :: SnailfishNumber -> PathToElement
pathToFirstToBeAdded sfn = fromMaybe [] $ givePathToExploding sfn >>= return . (`givePathToFirstRegularLeft` sfn)

explodeSfn :: SnailfishNumber -> SnailfishNumber
explodeSfn = undefined

hasRegularNumberGreaterThanNine :: SnailfishNumberElement -> Bool
hasRegularNumberGreaterThanNine (N val) = val > 9
hasRegularNumberGreaterThanNine (P (val1,val2)) = hasRegularNumberGreaterThanNine val1 || hasRegularNumberGreaterThanNine val2

needsToSplit :: SnailfishNumber -> Bool
needsToSplit = uncurry (||) . both hasRegularNumberGreaterThanNine

splitSfn :: SnailfishNumber -> SnailfishNumber
splitSfn = undefined

testInputs :: [String]
testInputs = ["[1,2]", "[[1,2],3]", "[9,[8,7]]", "[[1,9],[8,5]]", "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]", "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]", "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"]

addingExample :: String
addingExample = "[1,[[[[5,6],4],3],2]]"
