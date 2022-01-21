import Data.Char(digitToInt,intToDigit)
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe, mapMaybe)

type ALU = (Int,Int,Int,Int)
type ALUString = (String,String,String,String)

type Z = (Variable,Variable)

extractPrevious :: Z -> Z
extractPrevious z@(n,_) =
  case n of
    (Add (Mul oldN (Val 26)) oldR) -> (oldN,oldR)
    _ -> z

f :: Int -> Bool -> Z -> Z
f round xEqualsW z@(n,r) =
  let (d,t1,t2) = fromMaybe (0,0,0) (lookup round constants)
      newR = reduceAdd (Var (['a'..] !! (round - 1))) (Val t2)
  in if d == 1
       then if xEqualsW
              then z
              else (reduceAdd (reduceMultiply n (Val 26)) r, newR)
       else if xEqualsW
              then extractPrevious z
              else (n, newR)

applyOneRound :: (Bool,(Int,Int,Int),Maybe (Int,Int)) -> Int -> (Int,Int) -> (Int,Int)
applyOneRound (xEqualsW,(d,t1,t2),mRange) w (n,r) =
  let tmpZ = (26 * n + r) `div` d
      newZ = if r + t1 == w
               then 26 * tmpZ + w + t2
               else tmpZ
  in (tmpZ `div` 26, tmpZ `mod` 26)

allPaths :: [[Bool]]
allPaths = [ [a,b,c,d,e,f,g,h,i,j,k,l,m,n] | a <- [True,False], b <- [True,False], c <- [True,False], d <- [True,False], e <- [True,False], f <- [True,False], g <- [True,False], h <- [True,False], i <- [True,False], j <- [True,False], k <- [True,False], l <- [True,False], m <- [True,False], n <- [True,False]]

removeImpossible :: [Bool] -> Maybe [(Bool,(Int,Int,Int))]
removeImpossible bs =
  let bvars = zip bs (map (\ n -> fromMaybe undefined (lookup n constants)) [1..14])
      layerCounts = scanl (\ count (b,(d,_,_)) -> if not b && d == 1 then count+1 else if b && d == 26 then max 0 (count-1) else count) 0 bvars
  in if last layerCounts == 0
       then Just bvars
       else Nothing

overlap :: (Int,Int) -> (Int,Int) -> (Int,Int)
overlap (x1,y1) (x2,y2) = (max x1 x2, min y1 y2)

shrinkRange :: (Int,Int) -> (Bool,(Int,Int,Int),Maybe (Int,Int)) -> (Bool,(Int,Int,Int),Maybe (Int,Int))
shrinkRange (minR,maxR) (b,vars@(_,_,t2),mRange) =
  let currentRange = fromMaybe (1,9) mRange
      acceptedRange = (minR - t2, maxR - t2)
      newR = overlap currentRange acceptedRange
  in (b,vars,Just newR)

modifyNextR :: [(Bool,(Int,Int,Int),Maybe (Int,Int))] -> (Bool,(Int,Int,Int),Maybe (Int,Int)) -> [(Bool,(Int,Int,Int),Maybe (Int,Int))]
modifyNextR = helper 0
  where
    helper _ [] current = []
    helper skipCount orig@(next@(xEqualsW,(d,t1,t2),mRange):rest) current@(currentB,(currentD,currentT1,currentT2),currentMRange)
      | not currentB = orig
      | xEqualsW = if d == 1
                     then next : helper skipCount rest current
                     else next : helper (skipCount+1) rest current
      | skipCount > 0 = next : helper (skipCount-1) rest current
      | otherwise = shrinkRange (1-currentT1, 9-currentT1) next : rest

allRangesAreValid :: [(Bool,(Int,Int,Int),Maybe (Int,Int))] -> Bool
allRangesAreValid = all (\ (_,_,mRange) ->  maybe True (uncurry (<=)) mRange)

tagWithRange :: [(Bool,(Int,Int,Int))] -> Maybe [(Bool,(Int,Int,Int),Maybe (Int,Int))]
tagWithRange = helper [] . reverse . map (\ (a,b) -> (a,b,Nothing))
  where
    helper acc [] = if allRangesAreValid acc then Just acc else Nothing
    helper acc (vars:rest) =
      let modified = modifyNextR rest vars
      in helper (vars:acc) modified

biggestFulfilling :: [(Bool,(Int,Int,Int),Maybe (Int,Int))] -> Maybe [Char]
biggestFulfilling = helper (0,0) []
  where
    helper _ acc [] = Just . reverse $ acc
    helper z@(n,r) acc (vars@(xEqualsW,(d,t1,t2),mRange):rest) =
      let usableRange = reverse . filter (\ w -> if xEqualsW then w == r + t2 else w /= r + t2) . (\ p -> [(fst p)..(snd p)]) $ fromMaybe (1,9) mRange
      in if null usableRange
           then Nothing
           else case helper (applyOneRound vars (head usableRange) z) ((intToDigit . head $ usableRange) : acc) rest of
                  Nothing -> if null (tail usableRange) then Nothing else helper z acc ((xEqualsW,(d,t1,t2), Just (head $ tail usableRange,last $ tail usableRange)):rest)
                  Just cs -> Just cs

allCombinationsWithOne :: a -> [a] -> [[a]]
allCombinationsWithOne a [] = [[a]]
allCombinationsWithOne a bs = map ((a:) . (:[])) bs

-- allCombinations :: [a] -> [a] -> [[a]]
-- allCombinations f s = concatMap (`allCombinationsWithOne` s) f

-- allCombinationsWithMany :: [a] -> [[a]] -> [[a]]
-- allCombinationsWithMany f [] = [f]
-- allCombinationsWithMany f s = concatMap (allCombinations f) s

allCombinations :: [a] -> [[a]] -> [[a]]
allCombinations ls [] = map (:[]) ls
allCombinations [] dl = []
allCombinations (h:rest) dl = map (h:) dl ++ allCombinations rest dl

buildAll :: [[Int]] -> [[Char]]
buildAll = foldr (allCombinations . map intToDigit) []

possibilities = buildAll . map (\ (small,big) -> [small..big]) $ [(1,5), (3,9), (6,9), (2,9), (1,9), (1,9), (1,2), (1,6), (1,9), (1,9), (1,7), (1,9), (1,9), (1,9)]

firstPart :: [Char]
firstPart = maximum $ mapMaybe (\ bs ->  removeImpossible bs >>= tagWithRange >>= biggestFulfilling) allPaths

data Expr a = Var Char
            | Val a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Mod (Expr a) (Expr a)
            | Eql (Expr a) (Expr a)

instance Show a => Show (Expr a) where
  show (Var v1) = "(Var " ++ show v1 ++ ")"
  show (Val v1) = "(Val " ++ show v1 ++ ")"
  show (Add v1 v2) = "(Add " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (Mul v1 v2) = "(Mul " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (Div v1 v2) = "(Div " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (Mod v1 v2) = "(Mod " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (Eql v1 v2) = "(Eql " ++ show v1 ++ " " ++ show v2 ++ ")"

instance Functor Expr where
  fmap f (Var c) = Var c
  fmap f (Val v) = Val (f v)
  fmap f (Add v1 v2) = Add (fmap f v1) (fmap f v2)
  fmap f (Mul v1 v2) = Mul (fmap f v1) (fmap f v2)
  fmap f (Div v1 v2) = Div (fmap f v1) (fmap f v2)
  fmap f (Mod v1 v2) = Mod (fmap f v1) (fmap f v2)
  fmap f (Eql v1 v2) = Eql (fmap f v1) (fmap f v2)

type Variable = Expr Int
type ALUVar = (Variable,Variable,Variable,Variable)

reduceAdd :: Variable -> Variable -> Variable
reduceAdd (Val x) (Val y) = Val (x+y)
reduceAdd x (Val 0) = x
reduceAdd (Val 0) y = y
reduceAdd (Add x@(Val _) s) y@(Val _) = Add (reduceAdd x y) s
reduceAdd (Add s x@(Val _)) y@(Val _) = Add s (reduceAdd x y)
reduceAdd x@(Val _) (Add y@(Val _) s) = Add (reduceAdd x y) s
reduceAdd x@(Val _) (Add s y@(Val _)) = Add s (reduceAdd x y)
reduceAdd v1 v2 = Add v1 v2

reduceMultiply :: Variable -> Variable -> Variable
reduceMultiply (Val x) (Val y) = Val (x*y)
reduceMultiply _ (Val 0) = Val 0
reduceMultiply (Val 0) _ = Val 0
reduceMultiply x (Val 1) = x
reduceMultiply (Val 1) y = y
reduceMultiply (Mul x@(Val _) s) y@(Val _) = Mul (reduceMultiply x y) s
reduceMultiply (Mul s x@(Val _)) y@(Val _) = Mul s (reduceMultiply x y)
reduceMultiply v1 v2 = Mul v1 v2

reduceDiv :: Variable -> Variable -> Variable
reduceDiv (Val x) (Val y) = Val (x `div` y)
reduceDiv x (Val 1) = x
reduceDiv x (Val y) =
  let (x1,x2) = getRange x
      small = x1 `div` y
      big = x2 `div` y
  in if small == big then Val small else Div x (Val y)
reduceDiv v1 v2 = Div v1 v2

reduceMod :: Variable -> Variable -> Variable
reduceMod (Val x) (Val y) = Val (x `mod` y)
reduceMod (Mul (Val x) s) (Val y) = if x == y then Val 0 else Mod (Mul (Val x) s) (Val y)
reduceMod (Mul s (Val x)) (Val y) = if x == y then Val 0 else Mod (Mul s (Val x)) (Val y)
reduceMod x (Val y) =
  let (_,x2) = getRange x
  in if x2 < y then x else Mod x (Val y)
reduceMod v1 v2 = Mod v1 v2

reduceEql :: Variable -> Variable -> Variable
reduceEql (Val x) (Val y) = if x == y then Val 1 else Val 0
reduceEql x y =
  let (x1,x2) = getRange x
      (y1,y2) = getRange y
  in if x1 > y2 || x2 < y1 then Val 0 else Eql x y

getRange :: Variable -> (Int,Int)
getRange (Var _) = (1,9)
getRange (Val n) = (n,n)
getRange (Add x y) = let (x1,y1) = getRange x; (x2,y2) = getRange y; in (x1+x2,y1+y2)
getRange (Mul x y) = let (x1,y1) = getRange x; (x2,y2) = getRange y; in (x1*x2,y1*y2)
getRange (Div x y) = let (x1,x2) = getRange x; (y1,y2) = getRange y; in (x1 `div` y2,x2 `div` y1)
getRange (Mod x y) = let (_,y2) = getRange y; in (0,y2-1)
getRange (Eql x y) = (0,1)

combineOneVar :: [Char] -> Char -> [Char]
combineOneVar [] newC = [newC]
combineOneVar (c:cs) newC
  | newC < c = newC : c : cs
  | newC == c = c : cs
  | otherwise = c : combineOneVar cs newC

combineVars :: [Char] -> [Char] -> [Char]
combineVars = foldl combineOneVar

hasVars :: Variable -> [Char]
hasVars (Var c) = [c]
hasVars (Val _) = []
hasVars (Add x y) = hasVars x `combineVars` hasVars y
hasVars (Mul x y) = hasVars x `combineVars` hasVars y
hasVars (Div x y) = hasVars x `combineVars` hasVars y
hasVars (Mod x y) = hasVars x `combineVars` hasVars y
hasVars (Eql x y) = hasVars x `combineVars` hasVars y

replaceAndReduce :: Char -> Int -> Variable -> Variable
replaceAndReduce givenC i (Var c) = if c == givenC then Val i else Var c
replaceAndReduce _ _ (Val x) = Val x
replaceAndReduce c i (Add x y) = reduceAdd (replaceAndReduce c i x) (replaceAndReduce c i y)
replaceAndReduce c i (Mul x y) = reduceMultiply (replaceAndReduce c i x) (replaceAndReduce c i y)
replaceAndReduce c i (Div x y) = reduceDiv (replaceAndReduce c i x) (replaceAndReduce c i y)
replaceAndReduce c i (Mod x y) = reduceMod (replaceAndReduce c i x) (replaceAndReduce c i y)
replaceAndReduce c i (Eql x y) = reduceEql (replaceAndReduce c i x) (replaceAndReduce c i y)

smallestWithKnownOutput :: Int -> Variable -> [(Int,Variable)]
smallestWithKnownOutput target (Var c) = [(target,Var c)]
smallestWithKnownOutput target (Val x) = [(target, Val x)]
smallestWithKnownOutput target (Add x (Val y)) = smallestWithKnownOutput (target - y) x
smallestWithKnownOutput target (Add (Val x) y) = smallestWithKnownOutput (target - x) y
smallestWithKnownOutput 0 (Add x y) =
  let (x1,x2) = getRange x
      (y1,y2) = getRange y
  in if x1 == 0 && y1 == 0 then [(0,x),(0,y)] else [(0,Add x y)]
smallestWithKnownOutput target (Add x y) = [(target,Add x y)]
smallestWithKnownOutput target (Mul x (Val y)) = smallestWithKnownOutput (target `div` y) x
smallestWithKnownOutput target (Mul (Val x) y) = smallestWithKnownOutput (target `div` x) y
smallestWithKnownOutput 0 (Mul x y) =
  let (x1,x2) = getRange x
      (y1,y2) = getRange y
  in if x1 == 0 && y1 > 0
       then [(0,x)]
       else if x1 > 0 && y1 == 0
              then [(0,y)]
              else [(0,Mul x y)]
smallestWithKnownOutput target (Mul x y) = [(target,Mul x y)]
smallestWithKnownOutput target (Div x y) = [(target,Div x y)]
smallestWithKnownOutput target (Mod x y) = [(target,Mod x y)]
smallestWithKnownOutput target (Eql x y) = [(target,Eql x y)]

constants :: [(Int,(Int,Int,Int))]
constants = [(1,(1,13,8))
            ,(2,(1,12,13))
            ,(3,(1,12,8))
            ,(4,(1,10,10))
            ,(5,(26,-11,12))
            ,(6,(26,-13,1))
            ,(7,(1,15,13))
            ,(8,(1,10,5))
            ,(9,(26,-2,10))
            ,(10,(26,-6,3))
            ,(11,(1,14,2))
            ,(12,(26,0,2))
            ,(13,(26,-15,12))
            ,(14,(26,-4,7))]

giveFinalZ :: [String] -> String -> Variable
giveFinalZ instr inp = (\ (_,_,_,z) -> z) . executeInstructionsV instr inp $ initialALUVar

giveNthY :: Int -> Variable -> Variable
giveNthY 1 = last . snd . deconstruct
giveNthY n = giveNthY (n-1) . head . snd . deconstruct

couldHaveComeEqual :: Int -> Int -> [Int]
couldHaveComeEqual z round =
  let (d,t1,_) = fromMaybe undefined (lookup round constants)
  in if d == 1 then [z] else map ((d * z +) . (\ n -> n - t1)) [1..9]

couldHaveComeNotEqual :: Int -> Int -> [Int]
couldHaveComeNotEqual z round =
  let (d,t1,t2) = fromMaybe undefined (lookup round constants)
      wT2 = filter (\ n -> n >= 0 && n `mod` 26 == 0) $ map (\ n -> z - t2 - n) [1..9]
      mRemovedWT2 = if null wT2 then Nothing else Just $ head wT2
  in maybe [] (\ val -> map ((val `div` 26) * d +) [0..25]) mRemovedWT2

couldHaveComeFrom :: Int -> Int -> ([Int],[Int])
couldHaveComeFrom z round = (couldHaveComeEqual z round, couldHaveComeNotEqual z round)

bruteForce :: [Char] -> Int -> Variable -> [(Char,[Int])]
bruteForce = undefined

initialALU :: ALU
initialALU = (0,0,0,0)

initialALUString :: ALUString
initialALUString = ("0","0","0","0")

initialALUVar :: ALUVar
initialALUVar = (Val 0,Val 0,Val 0,Val 0)

input :: IO [String]
input = lines <$> readFile "day_24_input.txt"

get :: Char -> ALU -> Int
get c (w,x,y,z) =
  case c of
    'w' -> w
    'x' -> x
    'y' -> y
    'z' -> z
    _   -> undefined

getS :: Char -> ALUString -> String
getS c (w,x,y,z) =
  case c of
    'w' -> w
    'x' -> x
    'y' -> y
    'z' -> z
    _   -> undefined

getV :: Char -> ALUVar -> Variable
getV c (w,x,y,z) =
  case c of
    'w' -> w
    'x' -> x
    'y' -> y
    'z' -> z
    _   -> undefined

set :: Char -> Int -> ALU -> ALU
set c n orig@(w,x,y,z) =
  case c of
    'w' -> (n,x,y,z)
    'x' -> (w,n,y,z)
    'y' -> (w,x,n,z)
    'z' -> (w,x,y,n)
    _   -> orig

setS :: Char -> String -> ALUString -> ALUString
setS c n orig@(w,x,y,z) =
  case c of
    'w' -> (n,x,y,z)
    'x' -> (w,n,y,z)
    'y' -> (w,x,n,z)
    'z' -> (w,x,y,n)
    _   -> orig

setV :: Char -> Variable -> ALUVar -> ALUVar
setV c n orig@(w,x,y,z) =
  case c of
    'w' -> (n,x,y,z)
    'x' -> (w,n,y,z)
    'y' -> (w,x,n,z)
    'z' -> (w,x,y,n)
    _   -> orig

giveVal :: String -> ALU -> Int
giveVal s alu =
  if length s == 1 && head s `elem` "wxyz"
    then get (head s) alu
    else read s

giveValS :: String -> ALUString -> String
giveValS s alu =
  if length s == 1 && head s `elem` "wxyz"
    then getS (head s) alu
    else s

giveValV :: String -> ALUVar -> Variable
giveValV s alu
  | length s == 1 && head s `elem` "wxyz" = getV (head s) alu
  | head s `elem` ['a'..'n'] = Var (head s)
  | otherwise = Val (read s)

readInput :: [String] -> ALU -> ALU
readInput [targetStr,val] alu = set (head targetStr) (giveVal val alu) alu
readInput _ alu = alu

readInputS :: [String] -> ALUString -> ALUString
readInputS [targetStr,val] alu = setS (head targetStr) (giveValS val alu) alu
readInputS _ alu = alu

readInputV :: [String] -> ALUVar -> ALUVar
readInputV [targetStr,val] alu = setV (head targetStr) (giveValV val alu) alu
readInputV _ alu = alu

add :: [String] -> ALU -> ALU
add inputs@[f,_] alu =
  let [a,b] = map (`giveVal` alu) inputs
  in set (head f) (a + b) alu
add _ alu = alu

addS :: [String] -> ALUString -> ALUString
addS inputs@[f,_] alu =
  let [a,b] = map (`giveValS` alu) inputs
  in setS (head f) (a ++ "+" ++ b) alu
addS _ alu = alu

addV :: [String] -> ALUVar -> ALUVar
addV inputs@[f,_] alu =
  let [a,b] = map (`giveValV` alu) inputs
  in setV (head f) (reduceAdd a b) alu
addV _ alu = alu

multiply :: [String] -> ALU -> ALU
multiply inputs@[f,_] alu = set (head f) (product (map (`giveVal` alu) inputs)) alu
multiply _ alu = alu

multiplyS :: [String] -> ALUString -> ALUString
multiplyS inputs@[f,_] alu =
  let [a,b] = map (`giveValS` alu) inputs
  in setS (head f) ("(" ++ a ++ ")" ++ "*" ++ "(" ++ b ++ ")") alu
multiplyS _ alu = alu

multiplyV :: [String] -> ALUVar -> ALUVar
multiplyV inputs@[f,_] alu =
  let [a,b] = map (`giveValV` alu) inputs
  in setV (head f) (reduceMultiply a b) alu
multiplyV _ alu = alu

divide :: [String] -> ALU -> ALU
divide inputs@[f,_] alu =
  let [a,b] = map (`giveVal` alu) inputs
  in if b == 0
       then alu
       else set (head f) (a `div` b) alu
divide _ alu = alu

divideS :: [String] -> ALUString -> ALUString
divideS inputs@[f,_] alu =
  let [a,b] = map (`giveValS` alu) inputs
  in setS (head f) ("(" ++ a ++ ")" ++  "div"  ++ "(" ++ b ++ ")") alu
divideS _ alu = alu

divideV :: [String] -> ALUVar -> ALUVar
divideV inputs@[f,_] alu =
  let [a,b] = map (`giveValV` alu) inputs
  in setV (head f) (reduceDiv a b) alu
divideV _ alu = alu

modulo :: [String] -> ALU -> ALU
modulo inputs@[f,_] alu =
  let [a,b] = map (`giveVal` alu) inputs
  in if a < 0 || b <= 0
       then alu
       else set (head f) (a `mod` b) alu
modulo _ alu = alu

moduloS :: [String] -> ALUString -> ALUString
moduloS inputs@[f,_] alu =
  let [a,b] = map (`giveValS` alu) inputs
  in setS (head f) ("(" ++ a ++ ")" ++ "mod" ++ "(" ++ b ++ ")") alu
moduloS _ alu = alu

moduloV :: [String] -> ALUVar -> ALUVar
moduloV inputs@[f,_] alu =
  let [a,b] = map (`giveValV` alu) inputs
  in setV (head f) (reduceMod a b) alu
moduloV _ alu = alu

areEqual :: [String] -> ALU -> ALU
areEqual inputs@[f,_] alu =
  let [a,b] = map (`giveVal` alu) inputs
  in set (head f) (if a == b then 1 else 0) alu
areEqual _ alu = alu

areEqualS :: [String] -> ALUString -> ALUString
areEqualS inputs@[f,_] alu =
  let [a,b] = map (`giveValS` alu) inputs
  in setS (head f) ("(" ++ a ++ ")" ++ "==" ++ "(" ++ b ++ ")") alu
areEqualS _ alu = alu

areEqualV :: [String] -> ALUVar -> ALUVar
areEqualV inputs@[f,_] alu =
  let [a,b] = map (`giveValV` alu) inputs
  in setV (head f) (reduceEql a b) alu
areEqualV _ alu = alu

runInstruction :: String -> ALU -> ALU
runInstruction s alu =
  let (instr:inputs) = splitOn " " s
  in case instr of
       "inp" -> readInput inputs alu
       "add" -> add inputs alu
       "mul" -> multiply inputs alu
       "div" -> divide inputs alu
       "mod" -> modulo inputs alu
       "eql" -> areEqual inputs alu
       _     -> alu

runInstructionS :: String -> ALUString -> ALUString
runInstructionS s alu =
  let (instr:inputs) = splitOn " " s
  in case instr of
       "inp" -> readInputS inputs alu
       "add" -> addS inputs alu
       "mul" -> multiplyS inputs alu
       "div" -> divideS inputs alu
       "mod" -> moduloS inputs alu
       "eql" -> areEqualS inputs alu
       _     -> alu

runInstructionV :: String -> ALUVar -> ALUVar
runInstructionV s alu =
  let (instr:inputs) = splitOn " " s
  in case instr of
       "inp" -> readInputV inputs alu
       "add" -> addV inputs alu
       "mul" -> multiplyV inputs alu
       "div" -> divideV inputs alu
       "mod" -> moduloV inputs alu
       "eql" -> areEqualV inputs alu
       _     -> alu

tagInputInstructions :: String -> [String] -> [String]
tagInputInstructions input [] = []
tagInputInstructions [] instructions = instructions
tagInputInstructions input@(c:cs) (instr:rest) =
  if take 3 instr == "inp"
    then (instr ++ " " ++ [c]) : tagInputInstructions cs rest
    else instr : tagInputInstructions input rest

executeInstructions :: [String] -> String -> ALU -> ALU
executeInstructions instructions input alu = foldl (flip runInstruction) alu (tagInputInstructions input instructions)

executeInstructionsS :: [String] -> String -> ALUString -> ALUString
executeInstructionsS instructions input alu = foldl (flip runInstructionS) alu (tagInputInstructions input instructions)

executeInstructionsV :: [String] -> String -> ALUVar -> ALUVar
executeInstructionsV instructions input alu = foldl (flip runInstructionV) alu (tagInputInstructions input instructions)

deconstruct :: Variable -> (String,[Variable])
deconstruct (Var c) = ("Var",[Var c])
deconstruct (Val x) = ("Val",[Val x])
deconstruct (Add x y) = ("Add",[x,y])
deconstruct (Mul x y) = ("Mul",[x,y])
deconstruct (Div x y) = ("Div",[x,y])
deconstruct (Mod x y) = ("Mod",[x,y])
deconstruct (Eql x y) = ("Eql",[x,y])

giveZ :: ALUString -> String
giveZ = getS 'z'

giveResult :: [String] -> String -> ALUString -> String
giveResult instr inp alu = giveZ $ executeInstructionsS instr inp alu

numberIsValid :: ALU -> Bool
numberIsValid alu = get 'z' alu == 0

validateModelNumber :: [String] -> String -> Bool
validateModelNumber instructions input = numberIsValid $ executeInstructions instructions input initialALU

numberToString :: Int -> String
numberToString = helper []
  where
    helper acc 0 = acc
    helper acc n = helper (intToDigit (n `mod` 10) : acc) (n `div` 10)

giveSmaller :: String -> String
giveSmaller input = reverse . helper $ reverse input
  where
    helper [] = []
    helper (c:cs) =
      if c == '1'
        then '9' : helper cs
        else intToDigit (digitToInt c - 1) : cs

firstMatch :: [String] -> String -> Int
firstMatch instructions "11111111111111" = 0
firstMatch instructions input =
  if validateModelNumber instructions input
    then read input
    else firstMatch instructions (giveSmaller input)

-- firstPart :: IO Int
-- firstPart = do instructions <- input
--                return $ firstMatch instructions (replicate 14 '9')

test :: IO Bool
test = (`validateModelNumber` testInput) <$> input

testInput :: String
testInput = "13579246899999"

negateInstr :: [String]
negateInstr = ["inp x", "mul x -1"]

threeTimesLarger :: [String]
threeTimesLarger = ["inp z", "inp x", "mul z 3", "eql z x"]

bitRepr :: [String]
bitRepr = ["inp w"
          ,"add z w"
          ,"mod z 2"
          ,"div w 2"
          ,"add y w"
          ,"mod y 2"
          ,"div w 2"
          ,"add x w"
          ,"mod x 2"
          ,"div w 2"
          ,"mod w 2"]
