import Data.Char(intToDigit)
import Data.List(delete)
import Data.Maybe(fromMaybe,mapMaybe)

type Config = [Bool]
type Constants = [(Int,Int,Int)]
type Connections = [Int]
type Range = [Int]

matchLayers :: Config -> Constants -> Maybe Config
matchLayers bs consts = if (== 0) . foldl (\ l (b,(d,_,_)) -> addLayer b d l) 0 $ zip bs consts
                          then Just bs
                          else Nothing
  where
    addLayer b d
      | d == 1 = if b then id else (+1)
      | b = \ l -> max 0 (l-1)
      | otherwise = id

connections :: Config -> Constants -> Connections
connections = helper 1 [0]
  where
    helper n rs bs consts =
      let b = bs !! (n-1)
          (d,_,_) = consts !! (n-1)
          newRs
            | not b = n: if d ==1 then rs else tail rs
            | d == 1 = rs
            | otherwise = if null $ tail rs then [0] else tail rs
      in if n == length bs then [head rs] else head rs : helper (n+1) newRs bs consts

constants :: [(Int,Int,Int)]
constants = [(1,13,8)
            ,(1,12,13)
            ,(1,12,8)
            ,(1,10,10)
            ,(26,-11,12)
            ,(26,-13,1)
            ,(1,15,13)
            ,(1,10,5)
            ,(26,-2,10)
            ,(26,-6,3)
            ,(1,14,2)
            ,(26,0,2)
            ,(26,-15,12)
            ,(26,-4,7)]

leaveOverlap :: [Int] -> [Int] -> [Int] -- both parameter lists are assumed sorted
leaveOverlap [] ys = []
leaveOverlap xs [] = []
leaveOverlap (x:xs) ys = if x `elem` ys then x : leaveOverlap xs ys else leaveOverlap xs ys

removeOverlap :: [Int] -> [Int] -> [Int]
removeOverlap xs ys = foldl (flip delete) ys xs

collectPairs :: Config -> Connections -> [(Int,Int)]
collectPairs = helper 1
  where
    helper _ [] _ = []
    helper n (b:bs) conns =
      if not b
        then helper (n+1) bs conns
        else (conns !! (n-1), n) : helper (n+1) bs conns

giveOffsets :: [(Int,Int)] -> Constants -> [(Int,Int,Int)]
giveOffsets [] _ = []
giveOffsets ((a,b):rest) consts =
  let t1 = if a == 0 then 0 else (\ (_,t1,_) -> t1) $ consts !! (a-1)
      t2 = (\ (_,_,t2) -> t2) $ consts !! (b-1)
      newEntry = (a,b,t1+t2)
  in newEntry : giveOffsets rest consts

shrinkRanges :: (Int,Int,Int) -> [(Int,Range)] -> [(Int,Range)]
shrinkRanges (a,b,offset) table = newTable
  where
    aRange = reverse . leaveOverlap [1..9] $ [(1-offset)..(9-offset)]
    bRange = reverse . leaveOverlap [1..9] $ [(1+offset)..(9+offset)]
    bApplied = (b,bRange) : table
    newTable = if a == 0
                 then bApplied
                 else (a,aRange) : bApplied

collectRanges :: [(Int,Range)] -> [Range]
collectRanges = helper 1
  where
    helper 15 _ = []
    helper n table = fromMaybe (reverse [1..9]) (lookup n table) : helper (n+1) table

ranges :: Config -> Constants -> Connections -> [Range]
ranges bs consts conns =
  let pairs = collectPairs bs conns
      offsets = giveOffsets pairs consts
      deducedRanges = foldl (flip shrinkRanges) [] offsets
  in collectRanges deducedRanges

replaceGiven :: Int -> a -> [a] -> [a] -- index is not checked to be valid
replaceGiven 0 el orig = el : tail orig
replaceGiven n el orig = head orig : replaceGiven (n-1) el (tail orig)

giveAffected :: Int -> Connections -> [Int]
giveAffected = helper 1
  where
    helper _ _ [] = []
    helper ind n (c:cs) =
      if c == n
        then ind : helper (ind+1) n cs
        else helper (ind+1) n cs

propagateChange :: Int -> Int -> Config -> Constants -> Connections -> [Range] -> [Range]
propagateChange round picked bs consts conns =
  let affected = giveAffected round conns
      offsets = giveOffsets (zip (repeat round) affected) consts
      modifyRange :: (Int,Int,Int) -> [Range] -> [Range]
      modifyRange (_,n,offset) = if bs !! (n-1)
                                   then (\ rs -> replaceGiven (n-1) (leaveOverlap [picked] (rs !! (n-1))) rs)
                                   else (\ rs -> replaceGiven (n-1) (removeOverlap [picked] (rs !! (n-1))) rs)
  in flip (foldl (flip modifyRange)) offsets

giveBiggest :: Config -> [Range] ->  Constants -> Connections -> Maybe [Char]
giveBiggest = helper 1
  where
    helper n bs ranges consts conns
      | any null ranges = Nothing
      | n == 15 = Just []
      | otherwise =
        let r = ranges !! (n-1)
            picked = head r
            tryNext = helper n bs (tail r : tail ranges) consts conns
            newRanges = propagateChange n picked bs consts conns ranges
            res = helper (n+1) bs newRanges consts conns
        in case res of
             Nothing -> tryNext
             Just val -> Just $ intToDigit picked : val

biggestValid :: Config -> Maybe [Char]
biggestValid bs =
  let conns = connections bs constants
      rs = ranges bs constants conns
  in giveBiggest bs rs constants conns

allConfigs :: [Config]
allConfigs = helper [[True],[False]]
  where
    helper ls =
      if (== 14) . length . head $ ls
        then ls
        else let trueBranch = map (True :) ls
                 falseBranch = map (False :) ls
             in helper $ trueBranch ++ falseBranch

firstPart :: String
firstPart = maximum $ mapMaybe biggestValid allConfigs
