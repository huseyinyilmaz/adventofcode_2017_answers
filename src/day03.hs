module Main where
import Data.Char (digitToInt)
import Data.List (sort)
import Control.Applicative((<|>))
import Data.Maybe (fromJust)

readNumber :: IO Int
readNumber = getLine >>= (return . read)

lastCircleSize :: Int -> Int
lastCircleSize n = head $ dropWhile (\ x -> x*x < n) [1,3..]

findDistance :: Int -> Int
findDistance n = distanceFromMiddle + halfSide
  where
    largeSize = (lastCircleSize n)
    smallSize = (largeSize - 2)
    side = largeSize -1
    halfSide = side `div` 2
    lastPoint = largeSize * largeSize
    firstPoint = (smallSize * smallSize)
    index = n - firstPoint
    locationOnSide = index `mod` side
    distanceFromMiddle = abs $ locationOnSide - halfSide

main :: IO ()
main = do
  n <- readNumber
  (putStrLn . show) n
  (putStrLn . show . findDistance) n
