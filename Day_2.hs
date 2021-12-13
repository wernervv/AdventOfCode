import Control.Monad

type Position = (Int,Int) -- (horizontal, vertical)
type Position2 = (Int, Int, Int) -- (horizontal, vertical, aim)

input :: IO [String]
input = return . lines =<< readFile "day_2_input.txt"

firstPart :: IO Int
firstPart = liftM ((\ (a,b) -> a*b) . foldl move (0,0)) $ input

move :: Position -> String -> Position
move pos instr = moveCmd amount pos
  where
    (cmd, amount) = (\ [x,y] -> (x, read y :: Int)) $ words instr
    moveCmd = case cmd of
      "forward" -> increaseHorizontal
      "down"    -> increaseVertical
      "up"      -> decreaseVertical

increaseHorizontal :: Int -> Position -> Position
increaseHorizontal amount (h,v) = (h + amount, v)

increaseVertical :: Int -> Position -> Position
increaseVertical amount (h,v) = (h, v + amount)

decreaseVertical :: Int -> Position -> Position
decreaseVertical amount (h,v) = (h, max (v - amount) 0)

secondPart :: IO Int
secondPart = liftM ((\ (a,b,c) -> a*b) . foldl move2 (0,0,0)) $ input

move2 :: Position2 -> String -> Position2
move2 pos instr = moveCmd amount pos
  where
    (cmd, amount) = (\ [x,y] -> (x, read y :: Int)) $ words instr
    moveCmd = case cmd of
      "forward" -> \ amount -> \ orig@(h,v,a) -> (adapt $ increaseVertical (a * amount)) . (adapt $ increaseHorizontal amount) $ orig
      "down"    -> increaseAim
      "up"      -> decreaseAim

adapt :: ((a,b) -> (d,e)) -> ((a,b,c) -> (d,e,c))
adapt f = \ (a,b,c) -> let (d,e) = f (a,b) in (d,e,c)

increaseAim :: Int -> Position2 -> Position2
increaseAim amount (h,v,a) = (h,v, a + amount)

decreaseAim :: Int -> Position2 -> Position2
decreaseAim amount (h,v,a) = (h,v, max (a - amount) 0)
