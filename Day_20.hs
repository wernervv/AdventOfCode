import Data.List.Split(chunksOf,splitOn)

type Algorithm = String
type Image = [String]
type Coord = (Int,Int)
type InfiniteImage = (Image,Char)

input :: IO [String]
input = lines <$> readFile "day_20_input.txt"

parseInput :: [String] -> (Algorithm,Image)
parseInput input =
  let [algorithml,rest] = splitOn [""] input
  in (head algorithml,rest)

subImageCoords :: Coord -> [Coord]
subImageCoords (a,b) = [(x,y) | y <- [(b-1)..(b+1)], x <- [(a-1)..(a+1)]]

maxIndeces :: Image -> Coord
maxIndeces i = (maxX,maxY)
  where
    maxX = flip (-) 1 . length . head $ i
    maxY = flip (-) 1 $ length i

isWithinImage :: Coord -> Image -> Bool
isWithinImage (x,y) i = x >= 0 && x <= maxX && y >= 0 && y <= maxY
  where
    (maxX,maxY) = maxIndeces i

pickOnePixel :: Coord -> Image -> Char -> Char
pickOnePixel c@(x,y) i outSidePixel = if isWithinImage c i then i !! y !! x else outSidePixel

pickPixels :: [Coord] -> Image -> Char -> String
pickPixels cs i outSidePixel = map (\ c -> pickOnePixel c i outSidePixel) cs

pixelsToNum :: String -> Int
pixelsToNum = helper 0
  where
    helper acc [] = acc
    helper acc (b:bs) = helper (2*acc + currentAsNum) bs
      where
        currentAsNum = if b == '#' then 1 else 0

toInfiniteImage :: Image -> Char -> InfiniteImage
toInfiniteImage i outSidePixel = (i,outSidePixel)

enhanceOnePixel :: InfiniteImage -> Algorithm -> Coord -> Char
enhanceOnePixel (i,o) a = (a !!) . pixelsToNum . (\ coords -> pickPixels coords i o) . subImageCoords

enhanceImage :: InfiniteImage -> Algorithm -> InfiniteImage
enhanceImage i a =
  let (maxX,maxY) = maxIndeces $ fst i
      newImage = chunksOf (maxX+3) [enhanceOnePixel i a (x, y) | y <- [(- 1) .. (maxY + 1)], x <- [(- 1) .. (maxX + 1)]]
      newOutSidePixel = enhanceOnePixel i a (-2,-2) -- sub image is not overlapping with the given image
  in (newImage,newOutSidePixel)

countLightPixels :: Image -> Int
countLightPixels = sum . map (length . filter (== '#'))

lightPixelsAfterTwoEnhancements :: (Algorithm,Image) -> Int
lightPixelsAfterTwoEnhancements (a,i) = countLightPixels . fst $ enhanceImage (enhanceImage (toInfiniteImage i '.') a) a

firstPart :: IO Int
firstPart = lightPixelsAfterTwoEnhancements . parseInput <$> input
